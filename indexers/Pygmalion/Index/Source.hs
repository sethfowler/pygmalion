{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Pygmalion.Index.Source
( runSourceAnalyses
, displayAST
) where

import qualified Clang (Inclusion(..))
import Clang hiding (Inclusion(..), SourceLocation)
import qualified Clang.CrossReference as XRef
import qualified Clang.Cursor as C
import qualified Clang.Diagnostic as Diag
import qualified Clang.File as File
import Control.Monad.State.Strict
import qualified Clang.Source as Source
import qualified Clang.String as CS
import Clang.TranslationUnit
import qualified Clang.Type as T
import Control.Applicative
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import Data.Typeable
import qualified Data.Vector as VV
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Storable as DVS
import qualified Safe

import Data.Bool.Predicate
import Pygmalion.Core
import Pygmalion.Database.Request
import Pygmalion.File
import Pygmalion.Log
import Pygmalion.RPC.Client

type FileCache = Map.IntMap (Bool, Int)
type USRHashCache = Map.IntMap Int
type CPPScopeStack = [CPPScope]

data CPPScope = CPPScope
  { csNodeHash     :: !Int
  , csScopeUSRHash :: !Int
  , csScopeName    :: !BU.ByteString
  }
  
data TULocation = TULocation
  { tlShouldIndex :: !Bool
  , tlFileHash    :: !SourceFileHash
  , tlLine        :: !SourceLine
  , tlCol         :: !SourceCol
  } deriving (Eq, Show)

data AnalysisState = AnalysisState
  { asConn          :: !RPCConnection
  , asDirtyFiles    :: !Set.IntSet
  , asFileCache     :: !FileCache
  , asUSRHashCache  :: !USRHashCache
  , asCPPScopeStack :: !CPPScopeStack
  , asUpdates       :: !(V.IOVector DBUpdate)
  , asUpdateCount   :: !Int
  }

type Analysis s a = ClangT s (StateT AnalysisState IO) a

runAnalysis :: b -> StateT b IO a -> IO a
runAnalysis = flip evalStateT

vecSize :: Int
vecSize = 1000 -- 100000

runSourceAnalyses :: CommandInfo -> RPCConnection -> IO ()
runSourceAnalyses ci conn = do
  initialVec <- liftIO $ V.unsafeNew vecSize
  let !initialState = AnalysisState conn Set.empty Map.empty Map.empty [] initialVec 0
  result <- try $ runAnalysis initialState $ withTranslationUnit ci $ \tu -> do
                    clangScope $ logDiagnostics tu
                    clangScope $ inclusionsAnalysis (stableHash $ ciSourceFile ci) tu
                    analysisScope $ defsAnalysis tu
  case result of
    Right _ -> return ()
    Left (ClangException e) -> void $ logWarn ("Clang exception: " ++ e)

displayAST :: CommandInfo -> IO ()
displayAST ci = do
  result <- try $ withTranslationUnit ci doDisplayAST
  case result of
    Right _                 -> return ()
    Left (ClangException e) -> logWarn ("Clang exception: " ++ e )

inclusionsAnalysis :: SourceFileHash -> TranslationUnit s' -> Analysis s ()
inclusionsAnalysis sfHash tu = do
    !ctx <- lift get
    incs <- getInclusions tu
    incs' <- mapM toInclusion $ DVS.toList incs
    dirtyIncs <- queryRPC $ rpcUpdateAndFindDirtyInclusions sfHash incs'
    -- We include the source file itself in the list of dirty files.
    -- TODO: What we should do here is construct the 'dirtyFiles'
    -- cache here instead of lazily. Then we can pass it around with
    -- ReaderT instead of explicitly. This will remove significant
    -- complexity from defsAnalysis.
    let dirtyFiles = sfHash `Set.insert` Set.fromList dirtyIncs
    lift $ put $! ctx { asDirtyFiles = dirtyFiles }

  where

    toInclusion (Clang.Inclusion file loc _) = do
      inc <- CS.unsafeUnpackByteString =<< File.getName file
      canonicalInc <- liftIO $ canonicalPath inc
      (f, _, _, _) <- Source.getSpellingLocation loc
      filename <- case f of Just f' -> File.getName f' >>= CS.unsafeUnpackByteString
                            Nothing -> return ""
      canonicalFile <- liftIO $ canonicalPath filename
      -- liftIO $ putStrLn $ "Got inclusion " ++ show canonicalInc ++ " included by " ++ show canonicalFile
      return $ Inclusion canonicalInc (stableHash canonicalFile)

analysisScope :: (forall s. Analysis s ()) -> Analysis s' ()
analysisScope f = clangScope $ do
  f
  !ctx <- lift get
  when (asUpdateCount ctx > 0) $ do
    finishedVec <- liftIO $ VV.unsafeFreeze $ V.unsafeSlice 0 (asUpdateCount ctx) (asUpdates ctx)
    sendRPC $ rpcSendUpdates finishedVec
    newVec <- liftIO $ V.unsafeNew vecSize
    lift $ put $! ctx { asUpdates = newVec, asUpdateCount = 0 }

sendUpdate :: DBUpdate -> Analysis s ()
sendUpdate up = do
  !ctx <- lift $ get
  liftIO $ V.unsafeWrite (asUpdates ctx) (asUpdateCount ctx) up
  let !newCount = (asUpdateCount ctx) + 1
  if (newCount == vecSize)
     then do finishedVec <- liftIO $ VV.unsafeFreeze (asUpdates ctx)
             sendRPC $ rpcSendUpdates finishedVec
             newVec <- liftIO $ V.unsafeNew vecSize
             lift $ put $! ctx { asUpdates = newVec, asUpdateCount = 0 }
             --liftIO $ putStrLn "Overflowed vector and had to send early"
     else lift $ put $! ctx { asUpdateCount = newCount }

defsAnalysis :: TranslationUnit s' -> Analysis s ()
defsAnalysis tu = do
  cursor <- getCursor tu
  kids <- getChildren cursor
  DVS.mapM_ (defsVisitor cursor) kids

defsVisitor :: Cursor s' -> Cursor s'' -> Analysis s ()
defsVisitor parent cursor = do
  loc <- getCursorLocation cursor
  -- TODO: What to do about inclusions that aren't normal inclusions?
  -- Ones that are intended to be multiply included, etc?
  when (tlShouldIndex loc) $ do
    let !cKind = C.getKind cursor
    scope <- updatedCPPScope parent cursor
    route loc scope cursor cKind
    
    -- Recurse (most of the time).
    let recurse     = DVS.mapM_ (defsVisitor cursor) =<< getChildren cursor
        fastRecurse = DVS.mapM_ fastVisitor =<< getParentedDescendants cursor
    case cKind of
      Cursor_MacroDefinition  -> return ()
      Cursor_Namespace        -> analysisScope recurse
      _                       -> analysisScope fastRecurse

fastVisitor :: ParentedCursor s' -> Analysis s ()
fastVisitor (ParentedCursor parent cursor) = do
  loc <- getCursorLocation cursor
  when (tlShouldIndex loc) $ do
    let !cKind = C.getKind cursor
    scope <- updatedCPPScope parent cursor
    route loc scope cursor cKind

route :: TULocation -> CPPScope -> Cursor s' -> CursorKind -> Analysis s ()
route loc s c k@Cursor_StructDecl                         = visitReferences loc s k c >> visitClassOverrides c >> visitDefinitions loc s k c
route loc s c k@Cursor_UnionDecl                          = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ClassDecl                          = visitReferences loc s k c >> visitClassOverrides c >> visitDefinitions loc s k c
route loc s c k@Cursor_EnumDecl                           = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_FieldDecl                          = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_EnumConstantDecl                   = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_FunctionDecl                       = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_VarDecl                            = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ParmDecl                           = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCInterfaceDecl                  = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCCategoryDecl                   = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCProtocolDecl                   = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCPropertyDecl                   = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCIvarDecl                       = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCInstanceMethodDecl             = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCClassMethodDecl                = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCImplementationDecl             = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCCategoryImplDecl               = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_TypedefDecl                        = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_CXXMethod                          = visitReferences loc s k c >> visitOverrides c >> visitDefinitions loc s k c
route loc s c k@Cursor_Namespace                          = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_Constructor                        = visitReferences loc s k c >> visitOverrides c >> visitDefinitions loc s k c
route loc s c k@Cursor_Destructor                         = visitReferences loc s k c >> visitOverrides c >> visitDefinitions loc s k c
route loc s c k@Cursor_ConversionFunction                 = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_TemplateTypeParameter              = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_NonTypeTemplateParameter           = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_TemplateTemplateParameter          = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_FunctionTemplate                   = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ClassTemplate                      = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ClassTemplatePartialSpecialization = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_NamespaceAlias                     = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_UsingDirective                     = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_UsingDeclaration                   = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_TypeAliasDecl                      = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCSynthesizeDecl                 = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCDynamicDecl                    = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_ObjCSuperClassRef                  = visitReferences loc s k c
route loc s c k@Cursor_ObjCProtocolRef                    = visitReferences loc s k c
route loc s c k@Cursor_ObjCClassRef                       = visitReferences loc s k c
route loc s c k@Cursor_TypeRef                            = visitReferences loc s k c
route loc s c k@Cursor_TemplateRef                        = visitReferences loc s k c
route loc s c k@Cursor_NamespaceRef                       = visitReferences loc s k c
route loc s c k@Cursor_MemberRef                          = visitReferences loc s k c
route loc s c k@Cursor_LabelRef                           = visitReferences loc s k c
route loc s c k@Cursor_OverloadedDeclRef                  = visitReferences loc s k c
route loc s c k@Cursor_DeclRefExpr                        = visitReferences loc s k c
route loc s c k@Cursor_MemberRefExpr                      = visitReferences loc s k c
route loc s c k@Cursor_CallExpr                           = visitReferences loc s k c
route loc s c k@Cursor_ObjCMessageExpr                    = visitReferences loc s k c
route loc s c k@Cursor_MacroDefinition                    = visitReferences loc s k c >> visitDefinitions loc s k c
route loc s c k@Cursor_MacroExpansion                     = visitReferences loc s k c
route loc s c k@Cursor_InclusionDirective                 = visitInclusions loc s k c
route _ _ _ _                                               = return ()

sendRPC :: RPC () -> Analysis s ()
sendRPC req = do
  ctx <- lift get
  liftIO $ runRPC req (asConn ctx)

queryRPC :: RPC a -> Analysis s a
queryRPC req = do
  ctx <- lift get
  liftIO $ runRPC req (asConn ctx)

visitInclusions :: TULocation -> CPPScope -> CursorKind -> Cursor s' -> Analysis s ()
visitInclusions loc scope cKind cursor = do
  -- Record inclusion directives.
  incFile <- C.getIncludedFile cursor
  (_, incFileHash) <- lookupFile incFile

  -- Determine the end of the extent of this cursor.
  extent <- C.getExtent cursor
  endLoc <- Source.getEnd extent
  (_, endLn, endCol, _) <- Source.getSpellingLocation endLoc

  -- Record.
  refId <- refHash incFileHash loc
  let !reference = ReferenceUpdate refId
                                   (tlFileHash loc)
                                   (tlLine loc)
                                   (tlCol loc)
                                   endLn
                                   endCol
                                   (toSourceKind cKind)
                                   0
                                   0
                                   (csScopeUSRHash scope)
                                   incFileHash
  sendUpdate (DBUpdateRef reference)
      
visitReferences :: TULocation -> CPPScope -> CursorKind -> Cursor s' -> Analysis s ()
visitReferences loc scope cKind cursor = do
  -- Record references.
  -- TODO: Ignore CallExpr children that start at the same
  -- position as the CallExpr. This always refers to the same
  -- thing as the CallExpr itself. We don't want to just ignore
  -- the CallExpr though, because e.g. constructor calls are
  -- represented as CallExprs with no children.
  -- TODO: Support LabelRefs.
  defC <- C.getDefinition cursor
  refC <- C.getReferenced cursor
  let !defIsNull = C.isInvalid (C.getKind defC)
      !refIsNull = C.isInvalid (C.getKind refC)

  unless (defIsNull && refIsNull) $ do
    -- Prefer definitions to references when available.
    referToC <- if defIsNull
                   then maybeLiftToTemplate refC
                   else maybeLiftToTemplate defC

    referToUSRHash <- getUSRHash referToC

    -- Determine the end of the extent of this cursor.
    extent <- C.getExtent cursor
    endLoc <- Source.getEnd extent
    (_, endLn, endCol, _) <- Source.getSpellingLocation endLoc

    -- Special analysis for CallExprs.
    refKind <- if cKind == Cursor_CallExpr then analyzeCall cursor
                                             else return $ toSourceKind cKind
    viaUSRHash <- if cKind == Cursor_CallExpr then callBaseUSRHash cursor
                                                else return 0

    -- Record location of reference for declaration lookups.
    declHash <- if refIsNull
                then return 0
                else refHash referToUSRHash =<< getCursorLocation refC

    -- Record.
    refId <- refHash referToUSRHash loc
    let !reference = ReferenceUpdate refId
                                     (tlFileHash loc)
                                     (tlLine loc)
                                     (tlCol loc)
                                     endLn
                                     endCol
                                     refKind
                                     viaUSRHash
                                     declHash
                                     (csScopeUSRHash scope)
                                     referToUSRHash
    sendUpdate (DBUpdateRef reference)

maybeLiftToTemplate :: ClangBase m => Cursor s -> ClangT s m (Cursor s)
maybeLiftToTemplate cursor = do
  templateCursor <- C.getTemplateForSpecialization cursor
  return $ if C.isInvalid (C.getKind templateCursor)
             then cursor
             else templateCursor
                                     
visitOverrides :: Cursor s' -> Analysis s ()
visitOverrides cursor = do
  -- Record method overrides.
  -- TODO: I seem to recall that in C++11 you can override
  -- constructors. Add support for that if so.
  overrides <- C.getOverriddenCursors cursor
  overrideUSRHashes <- DVS.mapM getUSRHash overrides
  usrHash <- getUSRHash cursor
  DVS.forM_ overrideUSRHashes $ \oUSRHash -> do
    let !override = Override usrHash oUSRHash
    sendUpdate (DBUpdateOverride override)

visitDefinitions :: TULocation -> CPPScope -> CursorKind -> Cursor s' -> Analysis s ()
visitDefinitions loc scope cKind cursor = do
  -- Make sure we actually have a definition. Why C.isDefinition isn't
  -- reliable, I really don't know. Typical libclang.
  isDef <- C.isDefinition cursor
  let isDefLike = cKind == Cursor_MacroDefinition
               || cKind == Cursor_VarDecl

  when (isDef || isDefLike) $ do
    -- Record definitions.
    -- TODO: Support labels.
    usrHash <- getUSRHash cursor
    name <- cursorName cursor
    semanticParent <- C.getSemanticParent cursor
    lexicalParent <- C.getLexicalParent cursor

    (!fqn, !context) <-
      if semanticParent /= lexicalParent
        then (,) <$> getSemanticScope semanticParent name <*> getUSRHash semanticParent
        else return (csScopeName scope <::> name, csScopeUSRHash scope)

    let !kind = toSourceKind cKind
        !def = DefUpdate fqn
                         usrHash
                         (tlFileHash loc)
                         (tlLine loc)
                         (tlCol loc)
                         kind
                         context

    sendUpdate (DBUpdateDef def)

visitClassOverrides :: Cursor s' -> Analysis s ()
visitClassOverrides thisClassC = do
    -- Record class inheritance ("overrides").
    kids <- getChildren thisClassC
    DVS.mapM_ go kids
  where
    go cursor = do
      let !cKind = C.getKind cursor
      case cKind of
        Cursor_CXXBaseSpecifier -> do
          thisClassUSRHash <- getUSRHash thisClassC
          defC <- C.getDefinition cursor
          baseUSRHash <- getUSRHash defC
          let !override = Override thisClassUSRHash baseUSRHash
          sendUpdate (DBUpdateOverride override)
        _ -> return ()

getUSRHash :: Cursor s' -> Analysis s Int
getUSRHash cursor = do
  !ctx <- lift get
  let !cursorHash = stableHash cursor
      hashCache = asUSRHashCache ctx

  case Map.lookup cursorHash hashCache of
    Just usrHash -> return usrHash
    Nothing      -> do !usrHash <- stableHash <$> (XRef.getUSR cursor >>= CS.unsafeUnpackByteString)
                       let !newCache = Map.insert cursorHash usrHash hashCache
                       lift $ put $! ctx { asUSRHashCache = newCache }
                       return usrHash
    
getCursorLocation :: Cursor s' -> Analysis s TULocation
getCursorLocation cursor = do
  (mayF, ln, col, _) <- C.getSpellingLocation cursor
  case mayF of
    Just f  -> do (shouldIndex, filenameHash) <- lookupFile f
                  return $ TULocation shouldIndex filenameHash ln col
    Nothing -> return $ TULocation False 0 ln col

getCursorLocation' :: ClangBase m => Cursor s' -> ClangT s m SourceLocation
getCursorLocation' cursor = do
  (mayF, ln, col, _) <- C.getSpellingLocation cursor
  file <- case mayF of
    Just f -> File.getName f >>= CS.unsafeUnpackByteString
    Nothing -> return ""
  canonicalFile <- liftIO $ canonicalPath file
  return $ SourceLocation canonicalFile ln col
  
lookupFile :: File s' -> Analysis s (Bool, SourceFileHash)
lookupFile file = do
  !ctx <- lift get
  let fileObjHash = stableHash file  -- This is a hash of the file _object_, not the name.
      fileCache = asFileCache ctx

  case Map.lookup fileObjHash fileCache of
    Just (shouldIndex, filenameHash) ->
      return (shouldIndex, filenameHash)
    Nothing -> do
      path <- CS.unsafeUnpackByteString =<< File.getName file
      canonicalFile <- liftIO $ canonicalPath path
      let !filenameHash = stableHash canonicalFile
          !shouldIndex = filenameHash `Set.member` (asDirtyFiles ctx)
          !newCache = Map.insert fileObjHash (shouldIndex, filenameHash) fileCache
      lift $ put $! ctx { asFileCache = newCache }
      return (shouldIndex, filenameHash)
  
analyzeCall :: Cursor s' -> Analysis s SourceKind
analyzeCall c = do
  isDynamicCall <- C.isDynamicCall c

  if isDynamicCall then do
    mayBase <- findCallBase c
    case mayBase of
      Just base -> do let !baseKind = C.getKind base
                      isVirtual <- isVirtualCall baseKind base
                      return $ if isVirtual then DynamicCallExpr else CallExpr
      Nothing   -> return CallExpr
  else 
    return CallExpr

-- This is a bit of a hack, for now. We pop off the first
-- MemberRefExpr (which refers to the called method itself - not
-- exactly useful) and then pop off as many UnexposedExpr's as we
-- can. The UnexposedExpr's represent things like pointer
-- dereferences. I'd be much happier if they were exposed so we
-- weren't fumbling in the dark, but this seems to do the job for now.
findCallBase :: ClangBase m => Cursor s' -> ClangT s m (Maybe (Cursor s))
findCallBase c = do
  mayChild <- firstChild c
  case mayChild of
    Just child
      | C.getKind child == Cursor_MemberRefExpr -> skipAllUnexposed child
      | C.getKind child == Cursor_UnexposedExpr -> skipAllUnexposed child
    _                                             -> return mayChild

skipAllUnexposed :: ClangBase m => Cursor s' -> ClangT s m (Maybe (Cursor s))
skipAllUnexposed c = do
  mayChild <- firstChild c
  case mayChild of
    Just child
      | C.getKind child == Cursor_UnexposedExpr -> skipAllUnexposed child
    _                                             -> return mayChild

firstChild :: ClangBase m => Cursor s' -> ClangT s m (Maybe (Cursor s))
firstChild c = do
  children <- getChildren c
  return $ if DVS.null children
             then Nothing
             else Just $ DVS.head children

callBaseUSRHash :: Cursor s' -> Analysis s USRHash
callBaseUSRHash c = do
  mayBase <- findCallBase c
  decl <- case mayBase of
            Just base -> C.getTypeDeclaration =<< underlyingType base
            Nothing   -> C.getTypeDeclaration =<< underlyingType c
  stableHash <$> getUSRHash decl

refHash :: Int -> TULocation -> Analysis s USRHash
refHash usrHash loc = return refHash'
  where
    refHash' = usrHash `stableHashWithSalt` tlFileHash loc
                       `stableHashWithSalt` tlLine loc
                       `stableHashWithSalt` tlCol loc
                      
updatedCPPScope :: Cursor s' -> Cursor s'' -> Analysis s CPPScope
updatedCPPScope parent cursor = do
  ctx <- lift get
  let !cursorHash = stableHash cursor
  let !parentHash = stableHash parent

  -- Pop to parent scope.
  let !poppedStack = dropWhile ((parentHash /=) . csNodeHash) (asCPPScopeStack ctx)
      !parentScope = Safe.headDef (CPPScope 0 0 "") poppedStack

  -- Push new scope.
  let !cKind = C.getKind cursor
  (scopeHash, scopeName) <-
    if isScopeCursorKind cKind
       then newScopeName parentScope cursor
       else return (csScopeUSRHash parentScope, csScopeName parentScope)

  let !newStack = (CPPScope cursorHash scopeHash scopeName) : poppedStack
  lift $ put $! ctx { asCPPScopeStack = newStack }
  return parentScope

newScopeName :: CPPScope -> Cursor s' -> Analysis s (Int, BU.ByteString)
newScopeName parentScope cursor = do
  name <- cursorName cursor
  usrHash <- getUSRHash cursor
  let parentName = csScopeName parentScope
  return (usrHash, parentName <::> name)

(<::>) :: BU.ByteString -> BU.ByteString -> BU.ByteString
(<::>) a b = if B.null a
                then b
                else B.intercalate "::" [a, b]

cursorName :: Cursor s' -> Analysis s BU.ByteString
cursorName c = C.getDisplayName c >>= CS.unsafeUnpackByteString >>= anonymize
  where anonymize s | B.null s  = return "<anonymous>"
                    | otherwise = return s

isScopeCursorKind :: CursorKind -> Bool
isScopeCursorKind Cursor_FunctionDecl = True
isScopeCursorKind Cursor_CXXMethod    = True
isScopeCursorKind Cursor_Constructor  = True
isScopeCursorKind Cursor_Destructor   = True
isScopeCursorKind Cursor_ClassDecl    = True
isScopeCursorKind Cursor_StructDecl   = True
isScopeCursorKind Cursor_EnumDecl     = True
isScopeCursorKind Cursor_UnionDecl    = True
isScopeCursorKind Cursor_Namespace    = True
isScopeCursorKind _                     = False

getSemanticScope :: Cursor s' -> BU.ByteString -> Analysis s BU.ByteString
getSemanticScope c name
  | C.isInvalid (C.getKind c) = return name
  | isScopeCursorKind (C.getKind c) = do
      semanticParent <- C.getSemanticParent c
      scopeName <- cursorName c
      getSemanticScope semanticParent $ scopeName <::> name
  | otherwise = do
      semanticParent <- C.getSemanticParent c
      getSemanticScope semanticParent name

logDiagnostics :: TranslationUnit s' -> Analysis s ()
logDiagnostics tu = do
    opts <- Just <$> Diag.defaultDisplayOptions
    dias <- Diag.getDiagnostics tu
    forM_ dias $ \dia -> do
      severity <- Diag.getSeverity dia
      when (isError severity) $ do
        diaStr <- Diag.formatDiagnostic opts dia >>= CS.unpack
        liftIO $ logInfo $ "Diagnostic: " ++ diaStr
  where
    isError = (== Diagnostic_Error) .||. (== Diagnostic_Fatal)

doDisplayAST :: TranslationUnit s -> Clang s ()
doDisplayAST tu = getCursor tu >>= dumpSubtree

dumpSubtree :: Cursor s' -> Clang s ()
dumpSubtree cursor = do
    dumpVisitor 0 cursor
    liftIO $ putStrLn "Finished recursing!"
  where
    dumpVisitor :: Int -> Cursor s' -> Clang s ()
    dumpVisitor i c = do dump i c
                         kids <- getChildren c
                         DVS.mapM_ (dumpVisitor $ i + 1) kids

    dump :: Int -> Cursor s' -> Clang s ()
    dump i c = do
      -- Get location.
      loc <- C.getLocation c
      (_, ln, col, _) <- Source.getSpellingLocation loc

      -- Get extent.
      extent <- C.getExtent c
      (_, startLn, startCol, _) <- Source.getStart extent >>= Source.getSpellingLocation
      (_, endLn, endCol, _) <- Source.getEnd extent >>= Source.getSpellingLocation

      -- Get metadata.
      name <- C.getDisplayName c >>= CS.unpack
      usr <- XRef.getUSR c >>= CS.unpack
      let cKind = C.getKind c
      kind <- C.getCursorKindSpelling cKind >>= CS.unpack

      -- Get definition metadata.
      defCursor <- maybeLiftToTemplate =<< C.getDefinition c
      defName <- C.getDisplayName defCursor >>= CS.unpack
      defUSR <- XRef.getUSR defCursor >>= CS.unpack
      refCursor <- maybeLiftToTemplate =<< C.getReferenced c
      refName <- C.getDisplayName refCursor >>= CS.unpack
      refUSR <- XRef.getUSR refCursor >>= CS.unpack
      refLoc <- getCursorLocation' refCursor
      let refFile = BU.toString $ slFile refLoc

      -- Get type.
      typ <- C.getType c
      sTyp <- T.getTypeSpelling typ >>= CS.unpack
      typKind <- T.getKind typ
      sTypKind <- T.getTypeKindSpelling typKind >>= CS.unpack

      -- Special stuff for CallExpr
      when (cKind == Cursor_CallExpr) $ do
        mayBase <- findCallBase c
        baseExpr <- case mayBase of
                      Just b  -> return b
                      Nothing -> C.nullCursor
        baseName <- C.getDisplayName baseExpr >>= CS.unpack
        let baseExprKind = C.getKind baseExpr
            baseCKindSpelling = show $ toSourceKind baseExprKind
        baseType <- C.getType baseExpr
        baseTypeKind <- T.getKind baseType 
        uType <- underlyingType baseExpr
        uTypeKind <- T.getKind uType
        isVirtual <- isVirtualCall baseExprKind baseExpr
        baseTypeSpelling <- T.getTypeSpelling baseType >>= CS.unpack
        baseKindSpelling <- T.getTypeKindSpelling baseTypeKind >>= CS.unpack
        uTypeSpelling <- T.getTypeSpelling uType >>= CS.unpack
        uKindSpelling <- T.getTypeKindSpelling uTypeKind >>= CS.unpack
        liftIO $ putStrLn $ replicate i ' ' ++ "[==] CallExpr base: " ++ baseName ++ "/" ++ baseCKindSpelling
                                            ++ " type: " ++ baseTypeSpelling ++ "/" ++ baseKindSpelling
                                            ++ " utype: " ++ uTypeSpelling ++ "/" ++ uKindSpelling
                                            ++ " virtual? " ++ show isVirtual

      -- Display.
      liftIO $ putStrLn $ replicate i ' ' ++ "[" ++ kind ++ "] " ++ name ++ " (" ++ usr ++ ") " ++
                          "{" ++ sTyp ++ "/" ++ sTypKind ++ "} @ " ++
                          "<" ++ show ln ++ "," ++ show col ++ "> " ++
                          show startLn ++ "," ++ show startCol ++ " -> " ++
                          show endLn ++ "," ++ show endCol ++ " " ++
                          "definition [" ++ defName ++ "/" ++ defUSR ++ "] " ++
                          "reference [" ++ refName ++ "/" ++ refFile ++ "%" ++ refUSR ++ "]"

underlyingType :: ClangBase m => Cursor s' -> ClangT s m (Type s)
underlyingType c = do
  t <- C.getType c
  tKind <- T.getKind t
  if tKind `elem` [Type_LValueReference, Type_RValueReference, Type_Pointer, Type_Unexposed, Type_Invalid] then
    T.getPointeeType t
  else
    return t
    
isVirtualCall :: ClangBase m => CursorKind -> Cursor s' -> ClangT s m Bool
isVirtualCall Cursor_CallExpr = doesRefKindRequireVirtualDispatch
                              <=< callBaseResultKind
isVirtualCall Cursor_MemberRefExpr = doesRefKindRequireVirtualDispatch
                                   <=< callBaseKind
isVirtualCall Cursor_DeclRefExpr = doesRefKindRequireVirtualDispatch
                                 <=< callBaseKind
isVirtualCall _ = const $ return True

doesRefKindRequireVirtualDispatch :: ClangBase m => TypeKind -> ClangT s m Bool
doesRefKindRequireVirtualDispatch = return . (`elem` vdKinds)
  where
    vdKinds = [Type_LValueReference,
               Type_RValueReference,
               Type_Pointer,
               Type_Unexposed,
               Type_Invalid]

callBaseKind :: ClangBase m => Cursor s' -> ClangT s m TypeKind
callBaseKind = T.getKind <=< C.getType <=< C.getReferenced

callBaseResultKind :: ClangBase m => Cursor s' -> ClangT s m TypeKind
callBaseResultKind c = do
  refT <- C.getType =<< C.getReferenced c
  refTKind <- T.getKind refT
  refT' <- if refTKind == Type_Pointer
              then T.getPointeeType refT
              else return refT
  T.getKind =<< T.getResultType refT'

withTranslationUnit :: ClangBase m => CommandInfo
                    -> (forall s. TranslationUnit s -> ClangT s m a) -> m a
withTranslationUnit ci f = 
    withCreateIndex False False $ \index -> do
      setGlobalOptions index globalOpt_ThreadBackgroundPriorityForAll
      result <- withParse index
                          (Just $ unSourceFile sf)
                          args
                          VV.empty
                          [TranslationUnit_DetailedPreprocessingRecord]
                          f
      maybe bail return result
  where
    args = map BU.toString (ciArgs ci)
    sf = ciSourceFile ci
    bail = throw . ClangException $ "Libclang couldn't parse " ++ unSourceFile sf

data ClangException = ClangException String
  deriving (Show, Typeable)
instance Exception ClangException

toSourceKind :: CursorKind -> SourceKind
toSourceKind Cursor_UnexposedDecl                      = UnexposedDecl
toSourceKind Cursor_StructDecl                         = StructDecl
toSourceKind Cursor_UnionDecl                          = UnionDecl
toSourceKind Cursor_ClassDecl                          = ClassDecl
toSourceKind Cursor_EnumDecl                           = EnumDecl
toSourceKind Cursor_FieldDecl                          = FieldDecl
toSourceKind Cursor_EnumConstantDecl                   = EnumConstantDecl
toSourceKind Cursor_FunctionDecl                       = FunctionDecl
toSourceKind Cursor_VarDecl                            = VarDecl
toSourceKind Cursor_ParmDecl                           = ParmDecl
toSourceKind Cursor_ObjCInterfaceDecl                  = ObjCInterfaceDecl
toSourceKind Cursor_ObjCCategoryDecl                   = ObjCCategoryDecl
toSourceKind Cursor_ObjCProtocolDecl                   = ObjCProtocolDecl
toSourceKind Cursor_ObjCPropertyDecl                   = ObjCPropertyDecl
toSourceKind Cursor_ObjCIvarDecl                       = ObjCIvarDecl
toSourceKind Cursor_ObjCInstanceMethodDecl             = ObjCInstanceMethodDecl
toSourceKind Cursor_ObjCClassMethodDecl                = ObjCClassMethodDecl
toSourceKind Cursor_ObjCImplementationDecl             = ObjCImplementationDecl
toSourceKind Cursor_ObjCCategoryImplDecl               = ObjCCategoryImplDecl
toSourceKind Cursor_TypedefDecl                        = TypedefDecl
toSourceKind Cursor_CXXMethod                          = CXXMethod
toSourceKind Cursor_Namespace                          = Namespace
toSourceKind Cursor_LinkageSpec                        = LinkageSpec
toSourceKind Cursor_Constructor                        = Constructor
toSourceKind Cursor_Destructor                         = Destructor
toSourceKind Cursor_ConversionFunction                 = ConversionFunction
toSourceKind Cursor_TemplateTypeParameter              = TemplateTypeParameter
toSourceKind Cursor_NonTypeTemplateParameter           = NonTypeTemplateParameter
toSourceKind Cursor_TemplateTemplateParameter          = TemplateTemplateParameter
toSourceKind Cursor_FunctionTemplate                   = FunctionTemplate
toSourceKind Cursor_ClassTemplate                      = ClassTemplate
toSourceKind Cursor_ClassTemplatePartialSpecialization = ClassTemplatePartialSpecialization
toSourceKind Cursor_NamespaceAlias                     = NamespaceAlias
toSourceKind Cursor_UsingDirective                     = UsingDirective
toSourceKind Cursor_UsingDeclaration                   = UsingDeclaration
toSourceKind Cursor_TypeAliasDecl                      = TypeAliasDecl
toSourceKind Cursor_ObjCSynthesizeDecl                 = ObjCSynthesizeDecl
toSourceKind Cursor_ObjCDynamicDecl                    = ObjCDynamicDecl
toSourceKind Cursor_CXXAccessSpecifier                 = CXXAccessSpecifier
toSourceKind Cursor_ObjCSuperClassRef                  = ObjCSuperClassRef
toSourceKind Cursor_ObjCProtocolRef                    = ObjCProtocolRef
toSourceKind Cursor_ObjCClassRef                       = ObjCClassRef
toSourceKind Cursor_TypeRef                            = TypeRef
toSourceKind Cursor_CXXBaseSpecifier                   = CXXBaseSpecifier
toSourceKind Cursor_TemplateRef                        = TemplateRef
toSourceKind Cursor_NamespaceRef                       = NamespaceRef
toSourceKind Cursor_MemberRef                          = MemberRef
toSourceKind Cursor_LabelRef                           = LabelRef
toSourceKind Cursor_OverloadedDeclRef                  = OverloadedDeclRef
toSourceKind Cursor_VariableRef                        = VariableRef
toSourceKind Cursor_InvalidFile                        = InvalidFile
toSourceKind Cursor_NoDeclFound                        = NoDeclFound
toSourceKind Cursor_NotImplemented                     = NotImplemented
toSourceKind Cursor_InvalidCode                        = InvalidCode
toSourceKind Cursor_UnexposedExpr                      = UnexposedExpr
toSourceKind Cursor_DeclRefExpr                        = DeclRefExpr
toSourceKind Cursor_MemberRefExpr                      = MemberRefExpr
toSourceKind Cursor_CallExpr                           = CallExpr
toSourceKind Cursor_ObjCMessageExpr                    = ObjCMessageExpr
toSourceKind Cursor_BlockExpr                          = BlockExpr
toSourceKind Cursor_IntegerLiteral                     = IntegerLiteral
toSourceKind Cursor_FloatingLiteral                    = FloatingLiteral
toSourceKind Cursor_ImaginaryLiteral                   = ImaginaryLiteral
toSourceKind Cursor_StringLiteral                      = StringLiteral
toSourceKind Cursor_CharacterLiteral                   = CharacterLiteral
toSourceKind Cursor_ParenExpr                          = ParenExpr
toSourceKind Cursor_UnaryOperator                      = UnaryOperator
toSourceKind Cursor_ArraySubscriptExpr                 = ArraySubscriptExpr
toSourceKind Cursor_BinaryOperator                     = BinaryOperator
toSourceKind Cursor_CompoundAssignOperator             = CompoundAssignOperator
toSourceKind Cursor_ConditionalOperator                = ConditionalOperator
toSourceKind Cursor_CStyleCastExpr                     = CStyleCastExpr
toSourceKind Cursor_CompoundLiteralExpr                = CompoundLiteralExpr
toSourceKind Cursor_InitListExpr                       = InitListExpr
toSourceKind Cursor_AddrLabelExpr                      = AddrLabelExpr
toSourceKind Cursor_StmtExpr                           = StmtExpr
toSourceKind Cursor_GenericSelectionExpr               = GenericSelectionExpr
toSourceKind Cursor_GNUNullExpr                        = GNUNullExpr
toSourceKind Cursor_CXXStaticCastExpr                  = CXXStaticCastExpr
toSourceKind Cursor_CXXDynamicCastExpr                 = CXXDynamicCastExpr
toSourceKind Cursor_CXXReinterpretCastExpr             = CXXReinterpretCastExpr
toSourceKind Cursor_CXXConstCastExpr                   = CXXConstCastExpr
toSourceKind Cursor_CXXFunctionalCastExpr              = CXXFunctionalCastExpr
toSourceKind Cursor_CXXTypeidExpr                      = CXXTypeidExpr
toSourceKind Cursor_CXXBoolLiteralExpr                 = CXXBoolLiteralExpr
toSourceKind Cursor_CXXNullPtrLiteralExpr              = CXXNullPtrLiteralExpr
toSourceKind Cursor_CXXThisExpr                        = CXXThisExpr
toSourceKind Cursor_CXXThrowExpr                       = CXXThrowExpr
toSourceKind Cursor_CXXNewExpr                         = CXXNewExpr
toSourceKind Cursor_CXXDeleteExpr                      = CXXDeleteExpr
toSourceKind Cursor_UnaryExpr                          = UnaryExpr
toSourceKind Cursor_ObjCStringLiteral                  = ObjCStringLiteral
toSourceKind Cursor_ObjCEncodeExpr                     = ObjCEncodeExpr
toSourceKind Cursor_ObjCSelectorExpr                   = ObjCSelectorExpr
toSourceKind Cursor_ObjCProtocolExpr                   = ObjCProtocolExpr
toSourceKind Cursor_ObjCBridgedCastExpr                = ObjCBridgedCastExpr
toSourceKind Cursor_PackExpansionExpr                  = PackExpansionExpr
toSourceKind Cursor_SizeOfPackExpr                     = SizeOfPackExpr
toSourceKind Cursor_LambdaExpr                         = LambdaExpr
toSourceKind Cursor_ObjCBoolLiteralExpr                = ObjCBoolLiteralExpr
toSourceKind Cursor_ObjCSelfExpr                       = ObjCSelfExpr
toSourceKind Cursor_UnexposedStmt                      = UnexposedStmt
toSourceKind Cursor_LabelStmt                          = LabelStmt
toSourceKind Cursor_CompoundStmt                       = CompoundStmt
toSourceKind Cursor_CaseStmt                           = CaseStmt
toSourceKind Cursor_DefaultStmt                        = DefaultStmt
toSourceKind Cursor_IfStmt                             = IfStmt
toSourceKind Cursor_SwitchStmt                         = SwitchStmt
toSourceKind Cursor_WhileStmt                          = WhileStmt
toSourceKind Cursor_DoStmt                             = DoStmt
toSourceKind Cursor_ForStmt                            = ForStmt
toSourceKind Cursor_GotoStmt                           = GotoStmt
toSourceKind Cursor_IndirectGotoStmt                   = IndirectGotoStmt
toSourceKind Cursor_ContinueStmt                       = ContinueStmt
toSourceKind Cursor_BreakStmt                          = BreakStmt
toSourceKind Cursor_ReturnStmt                         = ReturnStmt
toSourceKind Cursor_AsmStmt                            = AsmStmt
toSourceKind Cursor_ObjCAtTryStmt                      = ObjCAtTryStmt
toSourceKind Cursor_ObjCAtCatchStmt                    = ObjCAtCatchStmt
toSourceKind Cursor_ObjCAtFinallyStmt                  = ObjCAtFinallyStmt
toSourceKind Cursor_ObjCAtThrowStmt                    = ObjCAtThrowStmt
toSourceKind Cursor_ObjCAtSynchronizedStmt             = ObjCAtSynchronizedStmt
toSourceKind Cursor_ObjCAutoreleasePoolStmt            = ObjCAutoreleasePoolStmt
toSourceKind Cursor_ObjCForCollectionStmt              = ObjCForCollectionStmt
toSourceKind Cursor_CXXCatchStmt                       = CXXCatchStmt
toSourceKind Cursor_CXXTryStmt                         = CXXTryStmt
toSourceKind Cursor_CXXForRangeStmt                    = CXXForRangeStmt
toSourceKind Cursor_SEHTryStmt                         = SEHTryStmt
toSourceKind Cursor_SEHExceptStmt                      = SEHExceptStmt
toSourceKind Cursor_SEHFinallyStmt                     = SEHFinallyStmt
toSourceKind Cursor_MSAsmStmt                          = MSAsmStmt
toSourceKind Cursor_NullStmt                           = NullStmt
toSourceKind Cursor_DeclStmt                           = DeclStmt
toSourceKind Cursor_OMPParallelDirective               = OMPParallelDirective
toSourceKind Cursor_TranslationUnit                    = TranslationUnit
toSourceKind Cursor_UnexposedAttr                      = UnexposedAttr
toSourceKind Cursor_IBActionAttr                       = IBActionAttr
toSourceKind Cursor_IBOutletAttr                       = IBOutletAttr
toSourceKind Cursor_IBOutletCollectionAttr             = IBOutletCollectionAttr
toSourceKind Cursor_CXXFinalAttr                       = CXXFinalAttr
toSourceKind Cursor_CXXOverrideAttr                    = CXXOverrideAttr
toSourceKind Cursor_AnnotateAttr                       = AnnotateAttr
toSourceKind Cursor_AsmLabelAttr                       = AsmLabelAttr
toSourceKind Cursor_PackedAttr                         = PackedAttr
toSourceKind Cursor_PreprocessingDirective             = PreprocessingDirective
toSourceKind Cursor_MacroDefinition                    = MacroDefinition
toSourceKind Cursor_MacroExpansion                     = MacroExpansion
toSourceKind Cursor_InclusionDirective                 = InclusionDirective
toSourceKind Cursor_ModuleImportDecl                   = ModuleImportDecl
