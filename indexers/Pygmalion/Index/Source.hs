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
import qualified Safe as Safe

import Data.Bool.Predicate
import Pygmalion.Core
import Pygmalion.Database.Request
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
  { tlShouldIndex :: Bool
  , tlFileHash    :: SourceFileHash
  , tlLine        :: SourceLine
  , tlCol         :: SourceCol
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
                    logDiagnostics tu
                    inclusionsAnalysis (hash $ ciSourceFile ci) tu
                    --addFileDefs conn ci dirtyFiles
                    analysisScope (defsAnalysis tu)
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
    !ctx <- lift get
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

analysisScope :: (forall s. Analysis s ()) -> Analysis s' ()
analysisScope = clangScope
{-
analysisScope f = clangScope $ do
  f
  ctx <- lift get
  when (asUpdateCount ctx > 0) $ do
    finishedVec <- liftIO $ VV.unsafeFreeze $ V.unsafeSlice 0 (asUpdateCount ctx) (asUpdates ctx)
    sendRPC $ rpcSendUpdates finishedVec
    newVec <- liftIO $ V.unsafeNew vecSize
    lift $ put $! ctx { asUpdates = newVec, asUpdateCount = 0 }
-}

sendUpdate :: DBUpdate -> Analysis s ()
sendUpdate _ = return ()
{-
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
-}

defsAnalysis :: TranslationUnit -> Analysis s ()
defsAnalysis tu = do
  cursor <- getCursor tu
  kids <- TV.getChildren cursor
  DVS.mapM_ (defsVisitor cursor) kids

defsVisitor :: C.Cursor -> C.Cursor -> Analysis s ()
defsVisitor parent cursor = do
  loc <- getCursorLocation cursor
  -- TODO: What to do about inclusions that aren't normal inclusions?
  -- Ones that are intended to be multiply included, etc?
  when (tlShouldIndex loc) $ do
    let !cKind = C.getKind cursor
    scope <- updatedCPPScope parent cursor
    route loc scope cKind cursor
    
    -- Recurse (most of the time).
    let recurse     = DVS.mapM_ (defsVisitor cursor) =<< TV.getChildren cursor
        fastRecurse = DVS.mapM_ (fastVisitor) =<< TV.getParentedDescendants cursor
    case cKind of
      C.Cursor_MacroDefinition  -> return ()
      C.Cursor_Namespace        -> analysisScope recurse
      _                         -> analysisScope fastRecurse

fastVisitor :: C.ParentedCursor -> Analysis s ()
fastVisitor (C.ParentedCursor parent cursor) = do
  loc <- getCursorLocation cursor
  when (tlShouldIndex loc) $ do
    let !cKind = C.getKind cursor
    scope <- updatedCPPScope parent cursor
    route loc scope cKind cursor

route :: TULocation -> CPPScope -> C.CursorKind -> C.Cursor -> Analysis s ()
route loc s k c
  | k == C.Cursor_UnexposedDecl                      = return ()
  | k == C.Cursor_StructDecl                         = visitReferences loc s k c >> visitClassOverrides c >> visitDefinitions loc s k c
  | k == C.Cursor_UnionDecl                          = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ClassDecl                          = visitReferences loc s k c >> visitClassOverrides c >> visitDefinitions loc s k c
  | k == C.Cursor_EnumDecl                           = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_FieldDecl                          = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_EnumConstantDecl                   = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_FunctionDecl                       = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_VarDecl                            = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ParmDecl                           = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCInterfaceDecl                  = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCCategoryDecl                   = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCProtocolDecl                   = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCPropertyDecl                   = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCIvarDecl                       = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCInstanceMethodDecl             = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCClassMethodDecl                = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCImplementationDecl             = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCCategoryImplDecl               = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_TypedefDecl                        = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_CXXMethod                          = visitReferences loc s k c >> visitOverrides c >> visitDefinitions loc s k c
  | k == C.Cursor_Namespace                          = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_LinkageSpec                        = return ()
  | k == C.Cursor_Constructor                        = visitReferences loc s k c >> visitOverrides c >> visitDefinitions loc s k c
  | k == C.Cursor_Destructor                         = visitReferences loc s k c >> visitOverrides c >> visitDefinitions loc s k c
  | k == C.Cursor_ConversionFunction                 = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_TemplateTypeParameter              = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_NonTypeTemplateParameter           = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_TemplateTemplateParameter          = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_FunctionTemplate                   = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ClassTemplate                      = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ClassTemplatePartialSpecialization = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_NamespaceAlias                     = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_UsingDirective                     = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_UsingDeclaration                   = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_TypeAliasDecl                      = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCSynthesizeDecl                 = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_ObjCDynamicDecl                    = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_CXXAccessSpecifier                 = return ()
  | k == C.Cursor_FirstDecl                          = return ()
  | k == C.Cursor_LastDecl                           = return ()
  | k == C.Cursor_FirstRef                           = return ()
  | k == C.Cursor_ObjCSuperClassRef                  = visitReferences loc s k c
  | k == C.Cursor_ObjCProtocolRef                    = visitReferences loc s k c
  | k == C.Cursor_ObjCClassRef                       = visitReferences loc s k c
  | k == C.Cursor_TypeRef                            = visitReferences loc s k c
  | k == C.Cursor_CXXBaseSpecifier                   = return ()
  | k == C.Cursor_TemplateRef                        = visitReferences loc s k c
  | k == C.Cursor_NamespaceRef                       = visitReferences loc s k c
  | k == C.Cursor_MemberRef                          = visitReferences loc s k c
  | k == C.Cursor_LabelRef                           = visitReferences loc s k c
  | k == C.Cursor_OverloadedDeclRef                  = visitReferences loc s k c
  | k == C.Cursor_LastRef                            = return ()
  | k == C.Cursor_FirstInvalid                       = return ()
  | k == C.Cursor_InvalidFile                        = return ()
  | k == C.Cursor_NoDeclFound                        = return ()
  | k == C.Cursor_NotImplemented                     = return ()
  | k == C.Cursor_InvalidCode                        = return ()
  | k == C.Cursor_LastInvalid                        = return ()
  | k == C.Cursor_FirstExpr                          = return ()
  | k == C.Cursor_UnexposedExpr                      = return ()
  | k == C.Cursor_DeclRefExpr                        = visitReferences loc s k c
  | k == C.Cursor_MemberRefExpr                      = visitReferences loc s k c
  | k == C.Cursor_CallExpr                           = visitReferences loc s k c
  | k == C.Cursor_ObjCMessageExpr                    = visitReferences loc s k c
  | k == C.Cursor_BlockExpr                          = return ()
  | k == C.Cursor_IntegerLiteral                     = return ()
  | k == C.Cursor_FloatingLiteral                    = return ()
  | k == C.Cursor_ImaginaryLiteral                   = return ()
  | k == C.Cursor_StringLiteral                      = return ()
  | k == C.Cursor_CharacterLiteral                   = return ()
  | k == C.Cursor_ParenExpr                          = return ()
  | k == C.Cursor_UnaryOperator                      = return ()
  | k == C.Cursor_ArraySubscriptExpr                 = return ()
  | k == C.Cursor_BinaryOperator                     = return ()
  | k == C.Cursor_CompoundAssignOperator             = return ()
  | k == C.Cursor_ConditionalOperator                = return ()
  | k == C.Cursor_CStyleCastExpr                     = return ()
  | k == C.Cursor_CompoundLiteralExpr                = return ()
  | k == C.Cursor_InitListExpr                       = return ()
  | k == C.Cursor_AddrLabelExpr                      = return ()
  | k == C.Cursor_StmtExpr                           = return ()
  | k == C.Cursor_GenericSelectionExpr               = return ()
  | k == C.Cursor_GNUNullExpr                        = return ()
  | k == C.Cursor_CXXStaticCastExpr                  = return ()
  | k == C.Cursor_CXXDynamicCastExpr                 = return ()
  | k == C.Cursor_CXXReinterpretCastExpr             = return ()
  | k == C.Cursor_CXXConstCastExpr                   = return ()
  | k == C.Cursor_CXXFunctionalCastExpr              = return ()
  | k == C.Cursor_CXXTypeidExpr                      = return ()
  | k == C.Cursor_CXXBoolLiteralExpr                 = return ()
  | k == C.Cursor_CXXNullPtrLiteralExpr              = return ()
  | k == C.Cursor_CXXThisExpr                        = return ()
  | k == C.Cursor_CXXThrowExpr                       = return ()
  | k == C.Cursor_CXXNewExpr                         = return ()
  | k == C.Cursor_CXXDeleteExpr                      = return ()
  | k == C.Cursor_UnaryExpr                          = return ()
  | k == C.Cursor_ObjCStringLiteral                  = return () 
  | k == C.Cursor_ObjCEncodeExpr                     = return () 
  | k == C.Cursor_ObjCSelectorExpr                   = return () 
  | k == C.Cursor_ObjCProtocolExpr                   = return () 
  | k == C.Cursor_ObjCBridgedCastExpr                = return () 
  | k == C.Cursor_PackExpansionExpr                  = return () 
  | k == C.Cursor_SizeOfPackExpr                     = return () 
  | k == C.Cursor_LastExpr                           = return () 
  | k == C.Cursor_FirstStmt                          = return () 
  | k == C.Cursor_UnexposedStmt                      = return () 
  | k == C.Cursor_LabelStmt                          = return () 
  | k == C.Cursor_CompoundStmt                       = return () 
  | k == C.Cursor_CaseStmt                           = return () 
  | k == C.Cursor_DefaultStmt                        = return () 
  | k == C.Cursor_IfStmt                             = return () 
  | k == C.Cursor_SwitchStmt                         = return () 
  | k == C.Cursor_WhileStmt                          = return () 
  | k == C.Cursor_DoStmt                             = return () 
  | k == C.Cursor_ForStmt                            = return () 
  | k == C.Cursor_GotoStmt                           = return () 
  | k == C.Cursor_IndirectGotoStmt                   = return () 
  | k == C.Cursor_ContinueStmt                       = return () 
  | k == C.Cursor_BreakStmt                          = return () 
  | k == C.Cursor_ReturnStmt                         = return () 
  | k == C.Cursor_AsmStmt                            = return () 
  | k == C.Cursor_ObjCAtTryStmt                      = return () 
  | k == C.Cursor_ObjCAtCatchStmt                    = return () 
  | k == C.Cursor_ObjCAtFinallyStmt                  = return () 
  | k == C.Cursor_ObjCAtThrowStmt                    = return () 
  | k == C.Cursor_ObjCAtSynchronizedStmt             = return () 
  | k == C.Cursor_ObjCAutoreleasePoolStmt            = return () 
  | k == C.Cursor_ObjCForCollectionStmt              = return () 
  | k == C.Cursor_CXXCatchStmt                       = return () 
  | k == C.Cursor_CXXTryStmt                         = return () 
  | k == C.Cursor_CXXForRangeStmt                    = return () 
  | k == C.Cursor_SEHTryStmt                         = return () 
  | k == C.Cursor_SEHExceptStmt                      = return () 
  | k == C.Cursor_SEHFinallyStmt                     = return () 
  | k == C.Cursor_NullStmt                           = return () 
  | k == C.Cursor_DeclStmt                           = return () 
  | k == C.Cursor_LastStmt                           = return () 
  | k == C.Cursor_TranslationUnit                    = return () 
  | k == C.Cursor_FirstAttr                          = return () 
  | k == C.Cursor_UnexposedAttr                      = return () 
  | k == C.Cursor_IBActionAttr                       = return () 
  | k == C.Cursor_IBOutletAttr                       = return () 
  | k == C.Cursor_IBOutletCollectionAttr             = return () 
  | k == C.Cursor_CXXFinalAttr                       = return () 
  | k == C.Cursor_CXXOverrideAttr                    = return () 
  | k == C.Cursor_AnnotateAttr                       = return () 
  | k == C.Cursor_LastAttr                           = return () 
  | k == C.Cursor_PreprocessingDirective             = return () 
  | k == C.Cursor_MacroDefinition                    = visitReferences loc s k c >> visitDefinitions loc s k c
  | k == C.Cursor_MacroExpansion                     = visitReferences loc s k c
  | k == C.Cursor_MacroInstantiation                 = visitReferences loc s k c
  | k == C.Cursor_InclusionDirective                 = visitInclusions loc s k c
  | k == C.Cursor_FirstPreprocessing                 = return ()
  | k == C.Cursor_LastPreprocessing                  = return ()
  | k == C.Cursor_ModuleImportDecl                   = return ()
  | k == C.Cursor_FirstExtraDecl                     = return ()
  | k == C.Cursor_LastExtraDecl                      = return ()
  | otherwise                                        = return ()  -- TODO: Log an error here.

doNothing :: C.CursorKind -> Analysis s ()
doNothing _ = return ()
{-# NOINLINE doNothing #-}

sendRPC :: RPC () -> Analysis s ()
{-
sendRPC req = do
  ctx <- lift get
  liftIO $ runRPC req (asConn ctx)
-}
sendRPC _ = return ()

queryRPC :: RPC a -> Analysis s a
queryRPC req = do
  ctx <- lift get
  liftIO $ runRPC req (asConn ctx)

visitInclusions :: TULocation -> CPPScope -> C.CursorKind -> C.Cursor -> Analysis s ()
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
      
visitReferences :: TULocation -> CPPScope -> C.CursorKind -> C.Cursor -> Analysis s ()
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
  let !defIsNull = C.isNullCursor defC
      !refIsNull = C.isNullCursor refC

  unless (defIsNull && refIsNull) $ do
    -- Prefer definitions to references when available.
    let referToC = if defIsNull then refC else defC
    referToUSRHash <- getUSRHash referToC

    -- Determine the end of the extent of this cursor.
    extent <- C.getExtent cursor
    endLoc <- Source.getEnd extent
    (_, endLn, endCol, _) <- Source.getSpellingLocation endLoc

    -- Special analysis for CallExprs.
    refKind <- if cKind == C.Cursor_CallExpr then analyzeCall cursor
                                             else return $ toSourceKind cKind
    viaUSRHash <- if cKind == C.Cursor_CallExpr then callBaseUSRHash cursor
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
    
visitOverrides :: C.Cursor -> Analysis s ()
visitOverrides cursor = do
  -- Record method overrides.
  -- TODO: I seem to recall that in C++11 you can override
  -- constructors. Add support for that if so.
  overrides <- C.getOverriddenCursors cursor
  overrideUSRHashes <- mapM getUSRHash overrides
  usrHash <- getUSRHash cursor
  forM_ overrideUSRHashes $ \oUSRHash -> do
    let !override = Override usrHash oUSRHash
    sendUpdate (DBUpdateOverride override)

visitDefinitions :: TULocation -> CPPScope -> C.CursorKind -> C.Cursor -> Analysis s ()
visitDefinitions loc scope cKind cursor = do
  -- Record definitions.
  -- TODO: Support labels.
  usrHash <- getUSRHash cursor
  name <- cursorName cursor
  let !fqn = csScopeName scope <::> name
  let !kind = toSourceKind cKind

  let !def = DefUpdate fqn
                       usrHash
                       (tlFileHash loc)
                       (tlLine loc)
                       (tlCol loc)
                       kind
                       (csScopeUSRHash scope)
  sendUpdate (DBUpdateDef def)

visitClassOverrides :: C.Cursor -> Analysis s ()
visitClassOverrides thisClassC = do
    -- Record class inheritance ("overrides").
    kids <- TV.getChildren thisClassC
    DVS.mapM_ go kids
  where
    go cursor = do
      let !cKind = C.getKind cursor
      case cKind of
        C.Cursor_CXXBaseSpecifier -> do
          thisClassUSRHash <- getUSRHash thisClassC
          defC <- C.getDefinition cursor
          baseUSRHash <- getUSRHash defC
          let !override = Override thisClassUSRHash baseUSRHash
          sendUpdate (DBUpdateOverride override)
        _ -> return ()

getUSRHash :: C.Cursor -> Analysis s Int
getUSRHash cursor = do
  !ctx <- lift get
  let !cursorHash = hash cursor
      hashCache = asUSRHashCache ctx

  case Map.lookup cursorHash hashCache of
    Just usrHash -> return usrHash
    Nothing      -> do !usrHash <- hash <$> (XRef.getUSR cursor >>= CS.unsafeUnpackByteString)
                       let !newCache = Map.insert cursorHash usrHash hashCache
                       lift $ put $! ctx { asUSRHashCache = newCache }
                       return usrHash
    
getCursorLocation :: C.Cursor -> Analysis s TULocation
getCursorLocation cursor = do
  (mayF, ln, col, _) <- C.getSpellingLocation cursor
  case mayF of
    Just f  -> do (shouldIndex, filenameHash) <- lookupFile f
                  return $ TULocation shouldIndex filenameHash ln col
    Nothing -> return $ TULocation False 0 ln col

getCursorLocation' :: C.Cursor -> Clang s SourceLocation
getCursorLocation' cursor = do
  (mayF, ln, col, _) <- C.getSpellingLocation cursor
  file <- case mayF of
    Just f -> File.getName f >>= CS.unsafeUnpackByteString
    Nothing -> return ""
  return $ SourceLocation file ln col
  
lookupFile :: File.File -> Analysis s (Bool, SourceFileHash)
lookupFile file = do
  !ctx <- lift get
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
    let !baseExprKind = C.getKind baseExpr
    isVirtual <- isVirtualCall baseExprKind baseExpr
    return $ if isVirtual then DynamicCallExpr else CallExpr
  else 
    return CallExpr

callBaseUSRHash :: C.Cursor -> Analysis s USRHash
callBaseUSRHash = go
  where
    go = return . hash
         -- <=< (\x -> (liftIO . putStrLn $ "Got base USR: " ++ (show x)) >> return x)
         <=< getUSRHash
         <=< C.getTypeDeclaration
         <=< underlyingType
         <=< C.getBaseExpression

refHash :: Int -> TULocation -> Analysis s USRHash
refHash usrHash loc = return refHash'
  where
    refHash' = usrHash `hashWithSalt` tlFileHash loc
                       `hashWithSalt` tlLine loc
                       `hashWithSalt` tlCol loc
                      
updatedCPPScope :: C.Cursor -> C.Cursor -> Analysis s CPPScope
updatedCPPScope parent cursor = do
  ctx <- lift get
  let !cursorHash = hash cursor
  let !parentHash = hash parent

  -- Pop to parent scope.
  let !poppedStack = dropWhile ((parentHash /=) . csNodeHash) (asCPPScopeStack ctx)
      !parentScope = Safe.headDef (CPPScope 0 0 "") poppedStack

  -- Push new scope.
  let !cKind = C.getKind cursor
  (scopeHash, scopeName) <- case cKind of
    C.Cursor_FunctionDecl -> newScopeName parentScope cursor
    C.Cursor_CXXMethod    -> newScopeName parentScope cursor
    C.Cursor_Constructor  -> newScopeName parentScope cursor
    C.Cursor_Destructor   -> newScopeName parentScope cursor
    C.Cursor_ClassDecl    -> newScopeName parentScope cursor
    C.Cursor_StructDecl   -> newScopeName parentScope cursor
    C.Cursor_EnumDecl     -> newScopeName parentScope cursor
    C.Cursor_UnionDecl    -> newScopeName parentScope cursor
    C.Cursor_Namespace    -> newScopeName parentScope cursor
    _                     -> return (csScopeUSRHash parentScope,
                                     csScopeName parentScope)

  let !newStack = (CPPScope cursorHash scopeHash scopeName) : poppedStack
  lift $ put $! ctx { asCPPScopeStack = newStack }
  return parentScope

newScopeName :: CPPScope -> C.Cursor -> Analysis s (Int, BU.ByteString)
newScopeName parentScope cursor = do
  name <- cursorName cursor
  usrHash <- getUSRHash cursor
  let parentName = csScopeName parentScope
  return (usrHash, parentName <::> name)

{-
fqn :: C.Cursor -> Analysis s Identifier 
fqn cursor = (B.intercalate "::" . reverse) <$> go cursor
  where go c = do isNull <- C.isNullCursor c
                  isTU <- C.getKind c >>= C.isTranslationUnit
                  if isNull || isTU then return [] else go' c
        go' c =  (:) <$> cursorName c <*> (C.getSemanticParent c >>= go)
-}

(<::>) :: BU.ByteString -> BU.ByteString -> BU.ByteString
(<::>) a b = if B.null a
                then b
                else B.intercalate "::" [a, b]

cursorName :: C.Cursor -> Analysis s BU.ByteString
cursorName c = C.getDisplayName c >>= CS.unsafeUnpackByteString >>= anonymize
  where anonymize s | B.null s  = return "<anonymous>"
                    | otherwise = return s

{-
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
-}

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
          let cKind = C.getKind c
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
            let baseExprKind = C.getKind baseExpr
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
toSourceKind C.Cursor_ModuleImportDecl                   = ModuleImportDecl
toSourceKind C.Cursor_FirstExtraDecl                     = FirstExtraDecl
toSourceKind C.Cursor_LastExtraDecl                      = LastExtraDecl
