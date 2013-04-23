{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Pygmalion.Analysis.Source
( runSourceAnalyses
, getLookupInfo
, LookupInfo (..)
, SourceAnalysisResult (..)
, SourceAnalysisState
, mkSourceAnalysisState
, displayAST
) where

import Clang.Alloc.Storable()
import qualified Clang.CrossReference as XRef
import qualified Clang.Cursor as C
import qualified Clang.Diagnostic as Diag
import qualified Clang.File as File
import Clang.Monad
import qualified Clang.Source as Source
import qualified Clang.String as CS
import Clang.TranslationUnit
import Clang.Traversal
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.IORef
import qualified Data.Text as T
import Data.Typeable

import Data.Bool.Predicate
import Pygmalion.Core
import Pygmalion.Log

data SourceAnalysisResult = SourceAnalysisResult
    { sarDefs       :: ![DefInfo]
    , sarOverrides  :: ![Override]
    , sarCallers    :: ![Caller]
    , sarRefs       :: ![Reference]
    , sarInclusions :: ![Inclusion]
    }

data SourceAnalysisState = SourceAnalysisState
    { defsVisitor       :: ChildVisitor
    , inclusionsVisitor :: InclusionVisitor
    , sourceFileRef     :: IORef SourceFile
    , defsRef           :: IORef [DefInfo]
    , overridesRef      :: IORef [Override]
    , callersRef        :: IORef [Caller]
    , refsRef           :: IORef [Reference]
    , inclusionsRef     :: IORef [Inclusion]
    , unitRef           :: IORef (Maybe TranslationUnit)
    }

mkSourceAnalysisState :: WorkingDirectory -> IO SourceAnalysisState
mkSourceAnalysisState wd = do
  newSFRef         <- newIORef $! (mkSourceFile "")
  newDefsRef       <- newIORef $! []
  newOverridesRef  <- newIORef $! []
  newCallersRef    <- newIORef $! []
  newRefsRef       <- newIORef $! []
  newInclusionsRef <- newIORef $! []
  newUnitRef       <- newIORef $! Nothing
  return $ SourceAnalysisState (defsVisitorImpl wd newDefsRef newCallersRef newUnitRef)
                               (inclusionsVisitorImpl wd newSFRef newInclusionsRef)
                               newSFRef
                               newDefsRef
                               newOverridesRef
                               newCallersRef
                               newRefsRef
                               newInclusionsRef
                               newUnitRef

runSourceAnalyses :: SourceAnalysisState -> CommandInfo -> IO (Maybe SourceAnalysisResult)
runSourceAnalyses sas ci@(CommandInfo sf _ _ _) = do
  writeIORef (sourceFileRef sas) $! sf
  writeIORef (defsRef sas)       $! []
  writeIORef (overridesRef sas)  $! []
  writeIORef (callersRef sas)    $! []
  writeIORef (refsRef sas)       $! []
  writeIORef (inclusionsRef sas) $! []
  result <- try $ withTranslationUnit ci $ \tu -> do
                    inclusionsAnalysis sas tu
                    defsAnalysis sas tu
  case result of
    Right _ -> Just <$> (SourceAnalysisResult <$> readIORef (defsRef sas)
                                              <*> readIORef (overridesRef sas)
                                              <*> readIORef (callersRef sas)
                                              <*> readIORef (refsRef sas)
                                              <*> readIORef (inclusionsRef sas))
    Left (ClangException e) -> logWarn ("Clang exception: " ++ e) >> return Nothing

data LookupInfo = GotDef DefInfo
                | GotDecl USR DefInfo
                | GotUSR USR
                | GotNothing
                deriving (Eq, Show)

getLookupInfo :: CommandInfo -> SourceLocation -> IO LookupInfo
getLookupInfo ci sl = do
  result <- try $ withTranslationUnit ci $ inspectIdentifier sl
  case result of
    Right r                 -> return r
    Left (ClangException e) -> logWarn ("Clang exception: " ++ e ) >> return GotNothing

displayAST :: CommandInfo -> IO ()
displayAST ci = do
  result <- try $ withTranslationUnit ci $ doDisplayAST
  case result of
    Right _                 -> return ()
    Left (ClangException e) -> logWarn ("Clang exception: " ++ e )

inclusionsAnalysis :: SourceAnalysisState -> TranslationUnit -> ClangApp ()
inclusionsAnalysis sas tu = void $ getInclusions tu (inclusionsVisitor sas)

inclusionsVisitorImpl :: WorkingDirectory -> IORef SourceFile -> IORef [Inclusion] -> InclusionVisitor
inclusionsVisitorImpl wd sfRef isRef file iStack = do
    ic <- File.getName file >>= CS.unpackText
    when (isLocalHeader wd ic) $ do
      sf <- liftIO $ readIORef sfRef
      case iStack of
        []       -> return ()                     -- The source file itself.
        (_ : []) -> liftIO . modifyIORef' isRef $! ((Inclusion sf ic True) :)
        (_ : _)  -> liftIO . modifyIORef' isRef $! ((Inclusion sf ic False) :)

isLocalHeader :: WorkingDirectory -> SourceFile -> Bool
isLocalHeader wd p = (wd `T.isPrefixOf`) .&&. (not . T.null) $ p

defsAnalysis :: SourceAnalysisState -> TranslationUnit -> ClangApp ()
defsAnalysis sas tu = do
    liftIO $ writeIORef (unitRef sas) $! (Just tu)
    cursor <- getCursor tu
    void $ visitChildren cursor (defsVisitor sas)

defsVisitorImpl :: WorkingDirectory -> IORef [DefInfo] -> IORef [Caller] -> IORef (Maybe TranslationUnit) -> ChildVisitor
defsVisitorImpl wd dsRef csRef tuRef cursor _ = do
  loc <- getCursorLocation cursor
  case inProject wd loc of
    True -> do  cKind <- C.getKind cursor
                when (cKind `elem` [C.Cursor_CallExpr, C.Cursor_MacroExpansion]) $ do
                  --liftIO $ logDebug "Got CallExpr"
                  refC <- C.getReferenced cursor
                  refIsNull <- C.isNullCursor refC
                  when (not refIsNull) $ do
                    refUSR <- XRef.getUSR refC >>= CS.unpackText
                    tu <- liftIO $ readIORef tuRef
                    parC <- parentFunction (fromJust tu) cursor
                    parUSR <- XRef.getUSR parC >>= CS.unpackText
                    caller <- return $! Caller loc parUSR refUSR
                    liftIO . modifyIORef' csRef $! (caller :)
                    --parName <- cursorName parC
                    --refName <- cursorName refC
                    --liftIO $ logDebug $ "Caller: " ++ (T.unpack parName) ++ " calls " ++ (T.unpack refName)
                cursorIsDef <- isDef cursor cKind
                when (cursorIsDef || cKind == C.Cursor_MacroDefinition) $ do
                  usr <- XRef.getUSR cursor >>= CS.unpackText
                  name <- fqn cursor
                  kind <- C.getCursorKindSpelling cKind >>= CS.unpackText
                  def <- return $! DefInfo name usr loc kind
                  liftIO . modifyIORef' dsRef $! (def :)
                return $ case cKind of
                            C.Cursor_FunctionDecl -> ChildVisit_Recurse
                            C.Cursor_CXXMethod    -> ChildVisit_Recurse
                            _                     -> ChildVisit_Recurse
    False -> return ChildVisit_Continue

getCursorLocation :: C.Cursor -> ClangApp SourceLocation
getCursorLocation cursor = do
  loc <- C.getLocation cursor
  (f, ln, col, _) <- Source.getSpellingLocation loc
  file <- case f of Just f' -> File.getName f' >>= CS.unpackText
                    Nothing -> return ""
  return $! SourceLocation file ln col

inProject :: WorkingDirectory -> SourceLocation -> Bool
inProject wd sl = (wd `T.isPrefixOf`) .&&. (not . T.null) $ slFile sl

isDef :: C.Cursor -> C.CursorKind -> ClangApp Bool
isDef c k = do
  q1 <- C.isDefinition c
  return $ q1 && not (k == C.Cursor_CXXAccessSpecifier)

fqn :: C.Cursor -> ClangApp Identifier 
fqn cursor = (T.intercalate "::" . reverse) <$> go cursor
  where go c = do isNull <- C.isNullCursor c
                  isTU <- C.getKind c >>= C.isTranslationUnit
                  if isNull || isTU then return [] else go' c
        go' c =  (:) <$> (cursorName c) <*> (C.getSemanticParent c >>= go)

parentFunction :: TranslationUnit -> C.Cursor -> ClangApp C.Cursor
parentFunction tu cursor = do
    isNull <- C.isNullCursor cursor
    cKind <- C.getKind cursor
    go cursor cKind isNull
  where
    go c _ isNull | isNull = return c
    go c k _ | k `elem` [C.Cursor_FunctionDecl, C.Cursor_CXXMethod, C.Cursor_TranslationUnit] = return c
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
                           _                       -> parentFunction tu srcCursor
    go c _ _ = C.getSemanticParent c >>= parentFunction tu

cursorName :: C.Cursor -> ClangApp Identifier
cursorName c = C.getDisplayName c >>= CS.unpackText >>= anonymize
  where anonymize s | T.null s  = return "<anonymous>"
                    | otherwise = return s

inspectIdentifier :: SourceLocation -> TranslationUnit -> ClangApp LookupInfo
inspectIdentifier (SourceLocation f ln col) tu = do
    dumpDiagnostics tu
    file <- File.getFile tu (unSourceFile f)
    loc <- Source.getLocation tu file ln col
    cursor <- Source.getCursor tu loc
    cKind <- C.getKind cursor

    -- kind <- C.getCursorKindSpelling cKind >>= CS.unpack
    -- liftIO $ logDebug $ "Cursor kind is " ++ kind

    defCursor <- if cKind == C.Cursor_MacroDefinition then return cursor
                                                      else C.getDefinition cursor
    isNullDef <- C.isNullCursor defCursor
    if isNullDef then do C.getReferenced cursor >>= reportIdentifier
                 else reportIdentifier defCursor
  where
    reportIdentifier cursor = do
      -- dumpSubtree cursor
      -- liftIO $ logDebug $ "In file: " ++ (T.unpack f) ++ ":" ++ (show ln) ++ ":" ++ (show col) ++ " got name: " ++ name ++ " usr: " ++ usr
      isNull <- C.isNullCursor cursor
      case isNull of
        False -> do usr <- XRef.getUSR cursor >>= CS.unpackText
                    kind <- C.getKind cursor 
                    cursorIsDef <- isDef cursor kind
                    di <- createDefInfo cursor usr kind
                    if cursorIsDef && isJust di
                      then return (GotDef $ fromJust di)
                      else if isJust di then return (GotDecl usr $ fromJust di)
                                        else return (GotUSR usr)
        True -> return GotNothing
    createDefInfo cursor usr k = do
      name <- C.getDisplayName cursor >>= CS.unpackText
      kind <- C.getCursorKindSpelling k >>= CS.unpackText
      loc <- C.getLocation cursor
      (df, dl, dc, _) <- Source.getSpellingLocation loc
      file <- case df of Just valid -> File.getName valid >>= CS.unpackText
                         Nothing    -> return ""
      return $ if (not $ T.null file) then Just (DefInfo name usr (SourceLocation file dl dc) kind)
                                      else Nothing

-- We need to decide on a policy, but it'd be good to figure out a way to let
-- the user display these, and maybe always display errors.
dumpDiagnostics :: TranslationUnit -> ClangApp ()
dumpDiagnostics tu = do
    opts <- Diag.defaultDisplayOptions
    dias <- Diag.getDiagnostics tu
    forM_ dias $ \dia -> do
      severity <- Diag.getSeverity dia
      when (isError severity) $ do
        diaStr <- Diag.formatDiagnostic opts dia >>= CS.unpack
        liftIO $ logInfo $ "Diagnostic: " ++ diaStr
  where
    isError = (== Diag.Diagnostic_Error) .||. (== Diag.Diagnostic_Fatal)

doDisplayAST :: TranslationUnit -> ClangApp ()
doDisplayAST tu = getCursor tu >>= dumpSubtree

dumpSubtree :: C.Cursor -> ClangApp ()
dumpSubtree cursor = do
    dump 0 cursor
    void $ visitChildren cursor (dumpVisitor 0)
    liftIO $ putStrLn "Finished recursing"
  where dumpVisitor :: Int -> ChildVisitor
        dumpVisitor i c _ = do dump i c
                               void $ visitChildren c (dumpVisitor $ i + 1)
                               return ChildVisit_Continue
        dump :: Int -> C.Cursor -> ClangApp ()
        dump i c = do
          -- Get extent.
          extent <- C.getExtent c
          (_, startLn, startCol, _) <- Source.getStart extent >>= Source.getSpellingLocation
          (_, endLn, endCol, _) <- Source.getEnd extent >>= Source.getSpellingLocation

          -- Get metadata.
          name <- C.getDisplayName c >>= CS.unpack
          usr <- XRef.getUSR c >>= CS.unpack
          kind <- C.getKind c >>= C.getCursorKindSpelling >>= CS.unpack

          -- Get definition metadata.
          defCursor <- C.getDefinition c
          defName <- C.getDisplayName defCursor >>= CS.unpack
          defUSR <- XRef.getUSR defCursor >>= CS.unpack
          refCursor <- C.getReferenced c
          refName <- C.getDisplayName refCursor >>= CS.unpack
          refUSR <- XRef.getUSR refCursor >>= CS.unpack

          -- Display.
          liftIO $ putStrLn $ (replicate i ' ') ++ "[" ++ kind ++ "] " ++ name ++ " (" ++ usr ++ ") @ " ++
                              (show startLn) ++ "," ++ (show startCol) ++ " -> " ++
                              (show endLn) ++ "," ++ (show endCol) ++ " " ++
                              "definition [" ++ defName ++ "/" ++ defUSR ++ "] " ++
                              "reference [" ++ refName ++ "/" ++ refUSR ++ "]"

withTranslationUnit :: CommandInfo -> (TranslationUnit -> ClangApp a) -> IO a
withTranslationUnit (CommandInfo sf _ (Command _ args) _) f = do
    withCreateIndex False False $ \index -> do
      setGlobalOptions index GlobalOpt_ThreadBackgroundPriorityForAll
      withParse index (Just . unSourceFile $ sf) clangArgs [] [TranslationUnit_DetailedPreprocessingRecord] f bail
  where
    bail = throw . ClangException $ "Libclang couldn't parse " ++ (unSourceFile sf)
    clangArgs = map T.unpack args
    -- FIXME: Is something along these lines useful? Internet claims so but this
    -- may be outdated information, as things seems to work OK without it.
    --clangArgs = map T.unpack ("-I/usr/local/Cellar/llvm/3.2/lib/clang/3.2/include" : args)

-- FIXME: Temporary until the new Haskell Platform comes out, which has this
-- built in.
modifyIORef' :: IORef a -> (a -> a) -> IO () 
modifyIORef' ref f = do 
 a <- readIORef ref 
 let a' = f a 
 seq a' $ writeIORef ref a' 

data ClangException = ClangException String
  deriving (Show, Typeable)
instance Exception ClangException
