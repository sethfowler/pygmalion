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

import qualified Clang.CrossReference as XRef
import qualified Clang.Cursor as C
import qualified Clang.Diagnostic as Diag
import qualified Clang.File as File
import Clang.Monad
import Control.Monad.State.Strict
import qualified Clang.Source as Source
import qualified Clang.String as CS
import Clang.TranslationUnit
import qualified Clang.Traversal as TV
import qualified Clang.Type as T
import Control.Applicative
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Data.Hashable
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import Data.Typeable
import qualified Data.Vector as VV
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Storable as DVS

import Data.Bool.Predicate
import Pygmalion.Core
import Pygmalion.Database.Request
import Pygmalion.Log
import Pygmalion.RPC.Client

data AnalysisState = AnalysisState
  { asConn        :: !RPCConnection
  , asDirtyFiles  :: !Set.IntSet
  , asFileCache   :: !FileCache
  , asUpdates     :: !(V.IOVector DBUpdate)
  , asUpdateCount :: !Int
  }

type Analysis s a = ClangT s (StateT AnalysisState IO) a

runAnalysis :: b -> StateT b IO a -> IO a
runAnalysis = flip evalStateT

vecSize :: Int
vecSize = 100000

runSourceAnalyses :: CommandInfo -> RPCConnection -> IO ()
runSourceAnalyses ci conn = do
  initialVec <- liftIO $ V.unsafeNew vecSize
  let initialState = AnalysisState conn Set.empty Map.empty initialVec 0
  result <- try $ runAnalysis initialState $ withTranslationUnit ci $ \tu -> do
                    logDiagnostics tu
                    inclusionsAnalysis (hash $ ciSourceFile ci) tu
                    --addFileDefs conn ci dirtyFiles
                    defsAnalysis ci tu
  case result of
    Right _ -> return ()
    Left (ClangException e) -> void $ logWarn ("Clang exception: " ++ e)

displayAST :: CommandInfo -> IO ()
displayAST ci = do
  result <- try $ withTranslationUnit ci doDisplayAST
  case result of
    Right _                 -> return ()
    Left (ClangException e) -> logWarn ("Clang exception: " ++ e )

inclusionsAnalysis :: SourceFileHash -> TranslationUnit -> Analysis s ()
inclusionsAnalysis sfHash tu = do
    ctx <- lift get
    {-
    clangIncPath <- mkSourceFile <$> liftIO libclangIncludePath
    incs <- DVS.filterM (prefixIsNot clangIncPath)
        =<< TV.getInclusions tu
    -}
    incs <- TV.getInclusions tu
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

    toInclusion (TV.Inclusion file loc _) = do
      inc <- CS.unsafeUnpackByteString =<< File.getName file
      (f, _, _, _) <- Source.getSpellingLocation loc
      filename <- case f of Just f' -> File.getName f' >>= CS.unsafeUnpackByteString
                            Nothing -> return ""
      --liftIO $ putStrLn $ "Got inclusion " ++ show inc ++ " included by " ++ show filename
      return $ Inclusion inc (hash filename)

    {-
    prefixIsNot clangIncSF (TV.Inclusion file _ _) =
      (not . B.isPrefixOf clangIncSF) <$> (CS.unsafeUnpackByteString =<< File.getName file)
    -}

{-
addFileDefs :: RPCConnection -> CommandInfo -> Set.IntSet -> Clang s ()
addFileDefs conn ci dirtyFiles = do
  -- Add a special definition for the beginning of each file we're indexing. This is
  -- referenced by inclusion directives.
  forM_ (Set.elems dirtyFiles) $ \sfHash ->
    let def = DefUpdate (ciSourceFile ci) sfHash sfHash 1 1 SourceFile 0
    liftIO $ runRPC (rpcFoundDef def) conn
-}
  
analysisScope :: (forall s. Analysis s ()) -> Analysis s' ()
analysisScope f = clangScope $ do
  f
  ctx <- lift get
  when (asUpdateCount ctx > 0) $ do
    finishedVec <- liftIO $ VV.unsafeFreeze $ V.unsafeSlice 0 (asUpdateCount ctx) (asUpdates ctx)
    sendRPC $ rpcSendUpdates finishedVec
    newVec <- liftIO $ V.unsafeNew vecSize
    lift $ put $! ctx { asUpdates = newVec, asUpdateCount = 0 }

sendUpdate :: DBUpdate -> Analysis s ()
sendUpdate up = do
  ctx <- lift $ get
  liftIO $ V.unsafeWrite (asUpdates ctx) (asUpdateCount ctx) up
  let newCount = (asUpdateCount ctx) + 1
  if (newCount == vecSize)
     then do finishedVec <- liftIO $ VV.unsafeFreeze (asUpdates ctx)
             sendRPC $ rpcSendUpdates finishedVec
             newVec <- liftIO $ V.unsafeNew vecSize
             lift $ put $! ctx { asUpdates = newVec, asUpdateCount = 0 }
             liftIO $ putStrLn $ "Overflowed vector and had to send early"
     else lift $ put $! ctx { asUpdateCount = newCount }

defsAnalysis :: CommandInfo -> TranslationUnit -> Analysis s ()
defsAnalysis ci tu = do
    cursor <- getCursor tu
    kids <- TV.getChildren cursor
    liftIO $ putStrLn $ show (ciSourceFile ci) ++ ": top-level nodes = " ++ show (DVS.length kids)
    go (DVS.splitAt groupSize kids)
  where
    go (!h, !t)
      | DVS.null h = return ()
      | DVS.null t = analysisScope $ DVS.mapM_ (defsVisitor tu) h
      | otherwise  = do analysisScope $ DVS.mapM_ (defsVisitor tu) h
                        go (DVS.splitAt groupSize t)

    groupSize = 10000

refKinds :: [C.CursorKind]
refKinds = [C.Cursor_CallExpr,
            C.Cursor_DeclRefExpr,
            C.Cursor_MemberRefExpr,
            C.Cursor_TypeRef,
            C.Cursor_MacroExpansion,
            C.Cursor_MacroDefinition,
            C.Cursor_CXXMethod]

overrideKinds :: [C.CursorKind]
overrideKinds = [C.Cursor_CXXMethod, C.Cursor_Destructor]

defKinds :: [C.CursorKind]
defKinds = [C.Cursor_MacroDefinition, C.Cursor_VarDecl]
           
defsVisitor :: TranslationUnit -> C.Cursor -> Analysis s ()
defsVisitor tu cursor = do
  loc <- getCursorLocation cursor
  -- TODO: What to do about inclusions that aren't normal inclusions?
  -- Ones that are intended to be multiply included, etc?
  when (tlShouldIndex loc) $ do
    cKind <- C.getKind cursor
    cursorIsDef <- isDef cursor cKind
    cursorIsRef <- C.isReference cKind
    cursorIsDecl <- C.isDeclaration cKind
    cursorIsPV <- if cKind == C.Cursor_CXXMethod
                  then C.isPureVirtualCppMethod cursor
                  else return False

    when (cKind == C.Cursor_InclusionDirective) $
      visitInclusions tu loc cKind cursor

    when ((cKind `elem` refKinds) || cursorIsDef || cursorIsRef || cursorIsDecl) $
      visitReferences tu loc cKind cursor
    
    when (cKind `elem` overrideKinds) $
      visitOverrides cursor

    when (cKind == C.Cursor_ClassDecl) $ do
      -- Record class inheritance ("overrides").
      kids <- TV.getChildren cursor
      DVS.mapM_ (classVisitor cursor) kids

    when ((cKind `elem` defKinds) || cursorIsDef || cursorIsPV) $
      visitDefinitions tu loc cKind cursor
    
    -- Recurse (most of the time).
    let recurse = DVS.mapM_ (defsVisitor tu) =<< TV.getChildren cursor
    case cKind of
      C.Cursor_MacroDefinition  -> return ()
      {-
      C.Cursor_FunctionDecl     -> analysisScope recurse
      C.Cursor_FunctionTemplate -> analysisScope recurse
      C.Cursor_CXXMethod        -> analysisScope recurse
      C.Cursor_Constructor      -> analysisScope recurse
      C.Cursor_Destructor       -> analysisScope recurse
      -}
      _                         -> recurse

sendRPC :: RPC () -> Analysis s ()
sendRPC req = do
  ctx <- lift get
  liftIO $ runRPC req (asConn ctx)

queryRPC :: RPC a -> Analysis s a
queryRPC req = do
  ctx <- lift get
  liftIO $ runRPC req (asConn ctx)

visitInclusions :: TranslationUnit -> TULocation -> C.CursorKind -> C.Cursor -> Analysis s ()
visitInclusions tu loc cKind cursor = do
  -- Record inclusion directives.
  incFile <- C.getIncludedFile cursor
  (_, incFileHash) <- lookupFile incFile

  -- Determine the end of the extent of this cursor.
  extent <- C.getExtent cursor
  endLoc <- Source.getEnd extent
  (_, endLn, endCol, _) <- Source.getSpellingLocation endLoc

  -- Determine the context.
  ctxC <- getContext tu cursor
  ctxUSR <- XRef.getUSR ctxC >>= CS.unsafeUnpackByteString

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
                                   (hash ctxUSR)
                                   incFileHash
  sendUpdate (DBUpdateRef reference)
      
visitReferences :: TranslationUnit -> TULocation -> C.CursorKind -> C.Cursor -> Analysis s ()
visitReferences tu loc cKind cursor = do
  -- Record references.
  -- TODO: Ignore CallExpr children that start at the same
  -- position as the CallExpr. This always refers to the same
  -- thing as the CallExpr itself. We don't want to just ignore
  -- the CallExpr though, because e.g. constructor calls are
  -- represented as CallExprs with no children.
  -- TODO: Support LabelRefs.
  defC <- C.getDefinition cursor
  defIsNull <- C.isNullCursor defC
  refC <- C.getReferenced cursor
  refIsNull <- C.isNullCursor refC

  unless (defIsNull && refIsNull) $ do
    -- Prefer definitions to references when available.
    let referToC = if defIsNull then refC else defC
    referToUSR <- XRef.getUSR referToC >>= CS.unsafeUnpackByteString

    -- Determine the end of the extent of this cursor.
    extent <- C.getExtent cursor
    endLoc <- Source.getEnd extent
    (_, endLn, endCol, _) <- Source.getSpellingLocation endLoc

    -- Determine the context.
    ctxC <- getContext tu cursor
    ctxUSR <- XRef.getUSR ctxC >>= CS.unsafeUnpackByteString

    -- Special analysis for CallExprs.
    refKind <- if cKind == C.Cursor_CallExpr then analyzeCall cursor
                                             else return $ toSourceKind cKind
    viaUSRHash <- if cKind == C.Cursor_CallExpr then callBaseUSRHash cursor
                                                else return 0

    -- Record location of reference for declaration lookups.
    let referToUSRHash = hash referToUSR
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
                                     (hash ctxUSR)
                                     (hash referToUSR)
    sendUpdate (DBUpdateRef reference)
    
visitOverrides :: C.Cursor -> Analysis s ()
visitOverrides cursor = do
  -- Record method overrides.
  -- TODO: I seem to recall that in C++11 you can override
  -- constructors. Add support for that if so.
  overrides <- C.getOverriddenCursors cursor
  overrideUSRs <- mapM (CS.unsafeUnpackByteString <=< XRef.getUSR) overrides
  usr <- XRef.getUSR cursor >>= CS.unsafeUnpackByteString
  forM_ overrideUSRs $ \oUSR -> do
    let !override = Override (hash usr) (hash oUSR)
    sendUpdate (DBUpdateOverride override)

visitDefinitions :: TranslationUnit -> TULocation -> C.CursorKind -> C.Cursor -> Analysis s ()
visitDefinitions tu loc cKind cursor = do
  -- Record definitions.
  -- TODO: Support labels.
  usr <- XRef.getUSR cursor >>= CS.unsafeUnpackByteString
  name <- fqn cursor
  let kind = toSourceKind cKind

  -- Determine the context.
  ctxC <- getContext' tu cursor
  ctxUSR <- XRef.getUSR ctxC >>= CS.unsafeUnpackByteString

  let !def = DefUpdate name
                       (hash usr)
                       (tlFileHash loc)
                       (tlLine loc)
                       (tlCol loc)
                       kind
                       (hash ctxUSR)
  sendUpdate (DBUpdateDef def)

classVisitor :: C.Cursor -> C.Cursor -> Analysis s ()
classVisitor thisClassC cursor = do
  cKind <- C.getKind cursor
  case cKind of
    C.Cursor_CXXBaseSpecifier -> do
      thisClassUSR <- XRef.getUSR thisClassC >>= CS.unsafeUnpackByteString
      defC <- C.getDefinition cursor
      baseUSR <- XRef.getUSR defC >>= CS.unsafeUnpackByteString
      let !override = Override (hash thisClassUSR) (hash baseUSR)
      sendUpdate (DBUpdateOverride override)
    _ -> return ()

type FileCache = Map.IntMap (Bool, Int)

data TULocation = TULocation
  { tlShouldIndex :: Bool
  , tlFileHash    :: SourceFileHash
  , tlLine        :: SourceLine
  , tlCol         :: SourceCol
  } deriving (Eq, Show)

getCursorLocation :: C.Cursor -> Analysis s TULocation
getCursorLocation cursor = do
  loc <- C.getLocation cursor
  (mayF, ln, col, _) <- Source.getSpellingLocation loc
  case mayF of
    Just f  -> do (shouldIndex, filenameHash) <- lookupFile f
                  return $ TULocation shouldIndex filenameHash ln col
    Nothing -> return $ TULocation False 0 ln col

getCursorLocation' :: C.Cursor -> Clang s SourceLocation
getCursorLocation' cursor = do
  loc <- C.getLocation cursor
  (f, ln, col, _) <- Source.getSpellingLocation loc
  file <- case f of Just f' -> File.getName f' >>= CS.unsafeUnpackByteString
                    Nothing -> return ""
  return $ SourceLocation file ln col
  
lookupFile :: File.File -> Analysis s (Bool, SourceFileHash)
lookupFile file = do
  ctx <- lift get
  fileObjHash <- File.hashFile file  -- This is a hash of the file _object_, not the name.
  let fileCache = asFileCache ctx

  case Map.lookup fileObjHash fileCache of
    Just (shouldIndex, filenameHash) ->
      return (shouldIndex, filenameHash)
    Nothing -> do
      !filenameHash <- hash <$> (CS.unsafeUnpackByteString =<< File.getName file)
      let !shouldIndex = filenameHash `Set.member` (asDirtyFiles ctx)
      let !newCache = Map.insert fileObjHash (shouldIndex, filenameHash) fileCache
      lift $ put $! ctx { asFileCache = newCache }
      return (shouldIndex, filenameHash)
  
analyzeCall :: C.Cursor -> Analysis s SourceKind
analyzeCall c = do
  isDynamicCall <- C.isDynamicCall c

  if isDynamicCall then do
    baseExpr <- C.getBaseExpression c
    baseExprKind <- C.getKind baseExpr
    isVirtual <- isVirtualCall baseExprKind baseExpr
    return $ if isVirtual then DynamicCallExpr else CallExpr
  else 
    return CallExpr

callBaseUSRHash :: C.Cursor -> Analysis s USRHash
callBaseUSRHash = go
  where
    go = return . hash
         -- <=< (\x -> (liftIO . putStrLn $ "Got base USR: " ++ (show x)) >> return x)
         <=< CS.unsafeUnpackByteString
         <=< XRef.getUSR
         <=< C.getTypeDeclaration
         <=< underlyingType
         <=< C.getBaseExpression

refHash :: Int -> TULocation -> Analysis s USRHash
refHash usrHash loc = return refHash'
  where
    refHash' = usrHash `hashWithSalt` tlFileHash loc
                       `hashWithSalt` tlLine loc
                       `hashWithSalt` tlCol loc
                      
isDef :: C.Cursor -> C.CursorKind -> Analysis s Bool
isDef c k = do
  q1 <- C.isDefinition c
  return $ q1 && (k /= C.Cursor_CXXAccessSpecifier)

fqn :: C.Cursor -> Analysis s Identifier 
fqn cursor = (B.intercalate "::" . reverse) <$> go cursor
  where go c = do isNull <- C.isNullCursor c
                  isTU <- C.getKind c >>= C.isTranslationUnit
                  if isNull || isTU then return [] else go' c
        go' c =  (:) <$> cursorName c <*> (C.getSemanticParent c >>= go)

getContext :: TranslationUnit -> C.Cursor -> Analysis s C.Cursor
getContext tu cursor = do
    isNull <- C.isNullCursor cursor
    cKind <- C.getKind cursor
    go cursor cKind isNull
  where
    go c _ isNull | isNull = return c
    go c k _ | k `elem` [C.Cursor_FunctionDecl, C.Cursor_CXXMethod, C.Cursor_Constructor, C.Cursor_Destructor, C.Cursor_TranslationUnit, C.Cursor_ClassDecl, C.Cursor_StructDecl, C.Cursor_EnumDecl, C.Cursor_Namespace] = return c
    go c k _ | k == C.Cursor_MacroExpansion = do
      extent <- C.getExtent c
      loc <- Source.getStart extent
      (f, ln, col, _) <- Source.getSpellingLocation loc
      case f of
         Nothing -> return c
         Just file -> do newLoc <- Source.getLocation tu file ln col
                         srcCursor <- Source.getCursor tu newLoc
                         srcCKind <- C.getKind srcCursor
                         case srcCKind of
                           C.Cursor_MacroExpansion -> return c  -- Will loop infinitely otherwise.
                           _                       -> getContext tu srcCursor
    go c _ _ = C.getSemanticParent c >>= getContext tu

getContext' :: TranslationUnit -> C.Cursor -> Analysis s C.Cursor
getContext' tu cursor = do
  isNull <- C.isNullCursor cursor
  cKind <- C.getKind cursor
  case (isNull, cKind) of
    (True, _)                         -> return cursor
    (False, C.Cursor_TranslationUnit) -> return cursor
    (False, _)                        -> C.getSemanticParent cursor >>= getContext tu

cursorName :: C.Cursor -> Analysis s Identifier
cursorName c = C.getDisplayName c >>= CS.unsafeUnpackByteString >>= anonymize
  where anonymize s | B.null s  = return "<anonymous>"
                    | otherwise = return s

logDiagnostics :: TranslationUnit -> Analysis s ()
logDiagnostics tu = do
    opts <- Diag.defaultDisplayOptions
    dias <- Diag.getDiagnostics tu
    forM_ dias $ \dia -> do
      severity <- Diag.getSeverity dia
      when (isError severity) $ do
        diaStr <- Diag.formatDiagnostic opts dia >>= CS.unpack
        liftIO $ logInfo $ "Diagnostic: " ++ diaStr
  where
    isError = (== Diag.Diagnostic_Error) .||. (== Diag.Diagnostic_Fatal)

doDisplayAST :: TranslationUnit -> Clang s ()
doDisplayAST tu = getCursor tu >>= dumpSubtree

dumpSubtree :: C.Cursor -> Clang s ()
dumpSubtree cursor = do
    dumpVisitor 0 cursor
    liftIO $ putStrLn "Finished recursing!"
  where dumpVisitor :: Int -> C.Cursor -> Clang s ()
        dumpVisitor i c = do dump i c
                             kids <- TV.getChildren c
                             DVS.mapM_ (dumpVisitor $ i + 1) kids
        dump :: Int -> C.Cursor -> Clang s ()
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
          cKind <- C.getKind c
          kind <- C.getCursorKindSpelling cKind >>= CS.unpack

          -- Get definition metadata.
          defCursor <- C.getDefinition c
          defName <- C.getDisplayName defCursor >>= CS.unpack
          defUSR <- XRef.getUSR defCursor >>= CS.unpack
          refCursor <- C.getReferenced c
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
          when (cKind == C.Cursor_CallExpr) $ do
            baseExpr <- C.getBaseExpression c
            baseName <- C.getDisplayName baseExpr >>= CS.unpack
            baseExprKind <- C.getKind baseExpr
            baseType <- C.getType baseExpr
            baseTypeKind <- T.getKind baseType 
            uType <- underlyingType baseExpr
            uTypeKind <- T.getKind uType
            isVirtual <- isVirtualCall baseExprKind baseExpr
            baseTypeSpelling <- T.getTypeSpelling baseType >>= CS.unpack
            baseKindSpelling <- T.getTypeKindSpelling baseTypeKind >>= CS.unpack
            uTypeSpelling <- T.getTypeSpelling uType >>= CS.unpack
            uKindSpelling <- T.getTypeKindSpelling uTypeKind >>= CS.unpack
            liftIO $ putStrLn $ replicate i ' ' ++ "[==] CallExpr base: " ++ baseName
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

underlyingType :: ClangBase m => C.Cursor -> ClangT s m T.Type
underlyingType c = do
  t <- C.getType c
  tKind <- T.getKind t
  if tKind `elem` [T.Type_LValueReference, T.Type_RValueReference, T.Type_Pointer, T.Type_Unexposed, T.Type_Invalid] then
    T.getPointeeType t
  else
    return t
    
isVirtualCall :: ClangBase m => C.CursorKind -> C.Cursor -> ClangT s m Bool
isVirtualCall k | k `elem` vcKinds =
    doesRefKindRequireVirtualDispatch <=< baseRefKind
  where
    vcKinds = [C.Cursor_CallExpr,
               C.Cursor_MemberRefExpr,
               C.Cursor_DeclRefExpr]
isVirtualCall _ = const $ return True

doesRefKindRequireVirtualDispatch :: ClangBase m => T.TypeKind -> ClangT s m Bool
doesRefKindRequireVirtualDispatch = return . (`elem` vdKinds)
  where
    vdKinds = [T.Type_LValueReference,
               T.Type_RValueReference,
               T.Type_Pointer,
               T.Type_Unexposed,
               T.Type_Invalid]

baseRefKind :: ClangBase m => C.Cursor -> ClangT s m T.TypeKind
baseRefKind = T.getKind <=< C.getType <=< C.getReferenced

withTranslationUnit :: ClangBase m => CommandInfo
                    -> (forall s. TranslationUnit -> ClangT s m a) -> m a
withTranslationUnit ci f = 
    withCreateIndex False False $ \index -> do
      clangIncludePath <- liftIO $ libclangIncludePath
      let clangIncludeArg = "-I" ++ clangIncludePath
      let args = (clangIncludeArg : map BU.toString (ciArgs ci))
      setGlobalOptions index GlobalOpt_ThreadBackgroundPriorityForAll
      withParse index (Just . unSourceFile $ sf) args [] [TranslationUnit_DetailedPreprocessingRecord] f bail
  where
    sf = ciSourceFile ci
    bail = throw . ClangException $ "Libclang couldn't parse " ++ unSourceFile sf

data ClangException = ClangException String
  deriving (Show, Typeable)
instance Exception ClangException

toSourceKind :: C.CursorKind -> SourceKind
toSourceKind C.Cursor_UnexposedDecl                      = UnexposedDecl
toSourceKind C.Cursor_StructDecl                         = StructDecl
toSourceKind C.Cursor_UnionDecl                          = UnionDecl
toSourceKind C.Cursor_ClassDecl                          = ClassDecl
toSourceKind C.Cursor_EnumDecl                           = EnumDecl
toSourceKind C.Cursor_FieldDecl                          = FieldDecl
toSourceKind C.Cursor_EnumConstantDecl                   = EnumConstantDecl
toSourceKind C.Cursor_FunctionDecl                       = FunctionDecl
toSourceKind C.Cursor_VarDecl                            = VarDecl
toSourceKind C.Cursor_ParmDecl                           = ParmDecl
toSourceKind C.Cursor_ObjCInterfaceDecl                  = ObjCInterfaceDecl
toSourceKind C.Cursor_ObjCCategoryDecl                   = ObjCCategoryDecl
toSourceKind C.Cursor_ObjCProtocolDecl                   = ObjCProtocolDecl
toSourceKind C.Cursor_ObjCPropertyDecl                   = ObjCPropertyDecl
toSourceKind C.Cursor_ObjCIvarDecl                       = ObjCIvarDecl
toSourceKind C.Cursor_ObjCInstanceMethodDecl             = ObjCInstanceMethodDecl
toSourceKind C.Cursor_ObjCClassMethodDecl                = ObjCClassMethodDecl
toSourceKind C.Cursor_ObjCImplementationDecl             = ObjCImplementationDecl
toSourceKind C.Cursor_ObjCCategoryImplDecl               = ObjCCategoryImplDecl
toSourceKind C.Cursor_TypedefDecl                        = TypedefDecl
toSourceKind C.Cursor_CXXMethod                          = CXXMethod
toSourceKind C.Cursor_Namespace                          = Namespace
toSourceKind C.Cursor_LinkageSpec                        = LinkageSpec
toSourceKind C.Cursor_Constructor                        = Constructor
toSourceKind C.Cursor_Destructor                         = Destructor
toSourceKind C.Cursor_ConversionFunction                 = ConversionFunction
toSourceKind C.Cursor_TemplateTypeParameter              = TemplateTypeParameter
toSourceKind C.Cursor_NonTypeTemplateParameter           = NonTypeTemplateParameter
toSourceKind C.Cursor_TemplateTemplateParameter          = TemplateTemplateParameter
toSourceKind C.Cursor_FunctionTemplate                   = FunctionTemplate
toSourceKind C.Cursor_ClassTemplate                      = ClassTemplate
toSourceKind C.Cursor_ClassTemplatePartialSpecialization = ClassTemplatePartialSpecialization
toSourceKind C.Cursor_NamespaceAlias                     = NamespaceAlias
toSourceKind C.Cursor_UsingDirective                     = UsingDirective
toSourceKind C.Cursor_UsingDeclaration                   = UsingDeclaration
toSourceKind C.Cursor_TypeAliasDecl                      = TypeAliasDecl
toSourceKind C.Cursor_ObjCSynthesizeDecl                 = ObjCSynthesizeDecl
toSourceKind C.Cursor_ObjCDynamicDecl                    = ObjCDynamicDecl
toSourceKind C.Cursor_CXXAccessSpecifier                 = CXXAccessSpecifier
toSourceKind C.Cursor_FirstDecl                          = FirstDecl
toSourceKind C.Cursor_LastDecl                           = LastDecl
toSourceKind C.Cursor_FirstRef                           = FirstRef
toSourceKind C.Cursor_ObjCSuperClassRef                  = ObjCSuperClassRef
toSourceKind C.Cursor_ObjCProtocolRef                    = ObjCProtocolRef
toSourceKind C.Cursor_ObjCClassRef                       = ObjCClassRef
toSourceKind C.Cursor_TypeRef                            = TypeRef
toSourceKind C.Cursor_CXXBaseSpecifier                   = CXXBaseSpecifier
toSourceKind C.Cursor_TemplateRef                        = TemplateRef
toSourceKind C.Cursor_NamespaceRef                       = NamespaceRef
toSourceKind C.Cursor_MemberRef                          = MemberRef
toSourceKind C.Cursor_LabelRef                           = LabelRef
toSourceKind C.Cursor_OverloadedDeclRef                  = OverloadedDeclRef
toSourceKind C.Cursor_LastRef                            = LastRef
toSourceKind C.Cursor_FirstInvalid                       = FirstInvalid
toSourceKind C.Cursor_InvalidFile                        = InvalidFile
toSourceKind C.Cursor_NoDeclFound                        = NoDeclFound
toSourceKind C.Cursor_NotImplemented                     = NotImplemented
toSourceKind C.Cursor_InvalidCode                        = InvalidCode
toSourceKind C.Cursor_LastInvalid                        = LastInvalid
toSourceKind C.Cursor_FirstExpr                          = FirstExpr
toSourceKind C.Cursor_UnexposedExpr                      = UnexposedExpr
toSourceKind C.Cursor_DeclRefExpr                        = DeclRefExpr
toSourceKind C.Cursor_MemberRefExpr                      = MemberRefExpr
toSourceKind C.Cursor_CallExpr                           = CallExpr
toSourceKind C.Cursor_ObjCMessageExpr                    = ObjCMessageExpr
toSourceKind C.Cursor_BlockExpr                          = BlockExpr
toSourceKind C.Cursor_IntegerLiteral                     = IntegerLiteral
toSourceKind C.Cursor_FloatingLiteral                    = FloatingLiteral
toSourceKind C.Cursor_ImaginaryLiteral                   = ImaginaryLiteral
toSourceKind C.Cursor_StringLiteral                      = StringLiteral
toSourceKind C.Cursor_CharacterLiteral                   = CharacterLiteral
toSourceKind C.Cursor_ParenExpr                          = ParenExpr
toSourceKind C.Cursor_UnaryOperator                      = UnaryOperator
toSourceKind C.Cursor_ArraySubscriptExpr                 = ArraySubscriptExpr
toSourceKind C.Cursor_BinaryOperator                     = BinaryOperator
toSourceKind C.Cursor_CompoundAssignOperator             = CompoundAssignOperator
toSourceKind C.Cursor_ConditionalOperator                = ConditionalOperator
toSourceKind C.Cursor_CStyleCastExpr                     = CStyleCastExpr
toSourceKind C.Cursor_CompoundLiteralExpr                = CompoundLiteralExpr
toSourceKind C.Cursor_InitListExpr                       = InitListExpr
toSourceKind C.Cursor_AddrLabelExpr                      = AddrLabelExpr
toSourceKind C.Cursor_StmtExpr                           = StmtExpr
toSourceKind C.Cursor_GenericSelectionExpr               = GenericSelectionExpr
toSourceKind C.Cursor_GNUNullExpr                        = GNUNullExpr
toSourceKind C.Cursor_CXXStaticCastExpr                  = CXXStaticCastExpr
toSourceKind C.Cursor_CXXDynamicCastExpr                 = CXXDynamicCastExpr
toSourceKind C.Cursor_CXXReinterpretCastExpr             = CXXReinterpretCastExpr
toSourceKind C.Cursor_CXXConstCastExpr                   = CXXConstCastExpr
toSourceKind C.Cursor_CXXFunctionalCastExpr              = CXXFunctionalCastExpr
toSourceKind C.Cursor_CXXTypeidExpr                      = CXXTypeidExpr
toSourceKind C.Cursor_CXXBoolLiteralExpr                 = CXXBoolLiteralExpr
toSourceKind C.Cursor_CXXNullPtrLiteralExpr              = CXXNullPtrLiteralExpr
toSourceKind C.Cursor_CXXThisExpr                        = CXXThisExpr
toSourceKind C.Cursor_CXXThrowExpr                       = CXXThrowExpr
toSourceKind C.Cursor_CXXNewExpr                         = CXXNewExpr
toSourceKind C.Cursor_CXXDeleteExpr                      = CXXDeleteExpr
toSourceKind C.Cursor_UnaryExpr                          = UnaryExpr
toSourceKind C.Cursor_ObjCStringLiteral                  = ObjCStringLiteral
toSourceKind C.Cursor_ObjCEncodeExpr                     = ObjCEncodeExpr
toSourceKind C.Cursor_ObjCSelectorExpr                   = ObjCSelectorExpr
toSourceKind C.Cursor_ObjCProtocolExpr                   = ObjCProtocolExpr
toSourceKind C.Cursor_ObjCBridgedCastExpr                = ObjCBridgedCastExpr
toSourceKind C.Cursor_PackExpansionExpr                  = PackExpansionExpr
toSourceKind C.Cursor_SizeOfPackExpr                     = SizeOfPackExpr
toSourceKind C.Cursor_LastExpr                           = LastExpr
toSourceKind C.Cursor_FirstStmt                          = FirstStmt
toSourceKind C.Cursor_UnexposedStmt                      = UnexposedStmt
toSourceKind C.Cursor_LabelStmt                          = LabelStmt
toSourceKind C.Cursor_CompoundStmt                       = CompoundStmt
toSourceKind C.Cursor_CaseStmt                           = CaseStmt
toSourceKind C.Cursor_DefaultStmt                        = DefaultStmt
toSourceKind C.Cursor_IfStmt                             = IfStmt
toSourceKind C.Cursor_SwitchStmt                         = SwitchStmt
toSourceKind C.Cursor_WhileStmt                          = WhileStmt
toSourceKind C.Cursor_DoStmt                             = DoStmt
toSourceKind C.Cursor_ForStmt                            = ForStmt
toSourceKind C.Cursor_GotoStmt                           = GotoStmt
toSourceKind C.Cursor_IndirectGotoStmt                   = IndirectGotoStmt
toSourceKind C.Cursor_ContinueStmt                       = ContinueStmt
toSourceKind C.Cursor_BreakStmt                          = BreakStmt
toSourceKind C.Cursor_ReturnStmt                         = ReturnStmt
toSourceKind C.Cursor_AsmStmt                            = AsmStmt
toSourceKind C.Cursor_ObjCAtTryStmt                      = ObjCAtTryStmt
toSourceKind C.Cursor_ObjCAtCatchStmt                    = ObjCAtCatchStmt
toSourceKind C.Cursor_ObjCAtFinallyStmt                  = ObjCAtFinallyStmt
toSourceKind C.Cursor_ObjCAtThrowStmt                    = ObjCAtThrowStmt
toSourceKind C.Cursor_ObjCAtSynchronizedStmt             = ObjCAtSynchronizedStmt
toSourceKind C.Cursor_ObjCAutoreleasePoolStmt            = ObjCAutoreleasePoolStmt
toSourceKind C.Cursor_ObjCForCollectionStmt              = ObjCForCollectionStmt
toSourceKind C.Cursor_CXXCatchStmt                       = CXXCatchStmt
toSourceKind C.Cursor_CXXTryStmt                         = CXXTryStmt
toSourceKind C.Cursor_CXXForRangeStmt                    = CXXForRangeStmt
toSourceKind C.Cursor_SEHTryStmt                         = SEHTryStmt
toSourceKind C.Cursor_SEHExceptStmt                      = SEHExceptStmt
toSourceKind C.Cursor_SEHFinallyStmt                     = SEHFinallyStmt
toSourceKind C.Cursor_NullStmt                           = NullStmt
toSourceKind C.Cursor_DeclStmt                           = DeclStmt
toSourceKind C.Cursor_LastStmt                           = LastStmt
toSourceKind C.Cursor_TranslationUnit                    = TranslationUnit
toSourceKind C.Cursor_FirstAttr                          = FirstAttr
toSourceKind C.Cursor_UnexposedAttr                      = UnexposedAttr
toSourceKind C.Cursor_IBActionAttr                       = IBActionAttr
toSourceKind C.Cursor_IBOutletAttr                       = IBOutletAttr
toSourceKind C.Cursor_IBOutletCollectionAttr             = IBOutletCollectionAttr
toSourceKind C.Cursor_CXXFinalAttr                       = CXXFinalAttr
toSourceKind C.Cursor_CXXOverrideAttr                    = CXXOverrideAttr
toSourceKind C.Cursor_AnnotateAttr                       = AnnotateAttr
toSourceKind C.Cursor_LastAttr                           = LastAttr
toSourceKind C.Cursor_PreprocessingDirective             = PreprocessingDirective
toSourceKind C.Cursor_MacroDefinition                    = MacroDefinition
toSourceKind C.Cursor_MacroExpansion                     = MacroExpansion
toSourceKind C.Cursor_MacroInstantiation                 = MacroInstantiation
toSourceKind C.Cursor_InclusionDirective                 = InclusionDirective
toSourceKind C.Cursor_FirstPreprocessing                 = FirstPreprocessing
toSourceKind C.Cursor_LastPreprocessing                  = LastPreprocessing
