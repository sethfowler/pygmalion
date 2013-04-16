{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Pygmalion.Analysis.Source
( runSourceAnalyses
, getLookupInfo
, LookupInfo (..)
, SourceAnalysisResult
, SourceAnalysisState
, mkSourceAnalysisState
, dumpSubtree -- Just to silence the warnings. Need to move this to another module.
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
import Pygmalion.Analysis.Extension
import Pygmalion.Core

type SourceAnalysisResult = (CommandInfo, [SourceFile], [DefInfo])

runSourceAnalyses :: SourceAnalysisState -> CommandInfo -> IO (Maybe ([SourceFile], [DefInfo]))
runSourceAnalyses sas ci@(CommandInfo sf _ _ _) = do
  writeIORef (includesRef sas) $! []
  writeIORef (defsRef sas) $! []
  writeIORef (sourceFileRef sas) $! sf
  result <- try $ withTranslationUnit ci $ do
                    --includesAnalysis includesRef wd
                    defsAnalysis sas
  case result of
    Right _ -> (,) <$> readIORef (includesRef sas) <*> readIORef (defsRef sas) >>= \x -> return $! Just $! x
    Left (ClangException e) -> putStrLn ("Clang exception: " ++ e) >> return Nothing

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
    Left (ClangException e) -> putStrLn ("Clang exception: " ++ e ) >> return GotNothing

includesAnalysis :: IORef [SourceFile] -> WorkingDirectory -> TranslationUnit -> ClangApp ()
includesAnalysis isRef wd tu = void $ getInclusions tu visitInclusions
  where
    visitInclusions :: InclusionVisitor
    visitInclusions file _  = do
      f <- File.getName file
      name <- CS.unpackText f
      when (isLocalHeader wd name) $
        liftIO . modifyIORef' isRef $! (name :)

isLocalHeader :: WorkingDirectory -> SourceFile -> Bool
isLocalHeader wd p = (wd `T.isPrefixOf`) .&&.
                     hasHeaderExtensionText .&&.
                     (not . T.null) $ p

defsAnalysis :: SourceAnalysisState -> TranslationUnit -> ClangApp ()
defsAnalysis sas tu = do
    cursor <- getCursor tu
    void $ visitChildren cursor (defsVisitor sas)

data SourceAnalysisState = SourceAnalysisState
    { defsVisitor :: ChildVisitor
    , defsRef :: IORef [DefInfo]
    , includesRef :: IORef [SourceFile]
    , sourceFileRef :: IORef SourceFile
    }

mkSourceAnalysisState :: WorkingDirectory -> IO SourceAnalysisState
mkSourceAnalysisState _ = do
  newDefsRef <- newIORef $! []
  newIncludesRef <- newIORef $! []
  newSFRef <- newIORef $! (mkSourceFile "")
  return $ SourceAnalysisState (defsVisitorImpl newDefsRef newSFRef) newDefsRef newIncludesRef newSFRef

defsVisitorImpl :: IORef [DefInfo] -> IORef SourceFile -> ChildVisitor
defsVisitorImpl dsRef sfRef cursor _ = do
  loc <- C.getLocation cursor
  (f, ln, col, _) <- Source.getSpellingLocation loc
  file <- case f of Just valid -> File.getName valid >>= CS.unpackText
                    Nothing    -> return ""
  thisFile <- liftIO $ readIORef sfRef
  case (file == thisFile) of
    True -> do  cKind <- C.getKind cursor
                cursorIsDef <- isDef cursor cKind
                when cursorIsDef $ do
                  usr <- XRef.getUSR cursor >>= CS.unpackText
                  name <- fqn cursor
                  kind <- C.getCursorKindSpelling cKind >>= CS.unpackText
                  def <- return $! DefInfo name usr
                                    (SourceLocation file ln col)
                                    kind
                  liftIO . modifyIORef' dsRef $! (def :)
                return $ case cKind of
                            C.Cursor_FunctionDecl -> ChildVisit_Continue
                            C.Cursor_CXXMethod    -> ChildVisit_Continue
                            _                     -> ChildVisit_Recurse
    False -> return ChildVisit_Continue

{-
-- Was the following; still evaluating the tradeoffs.
-- It probably does NOT make sense to store definitions that must necessarily
-- occur in the same file, because we have to parse the file anyway to get the
-- USR. Should refactor things to support that flow.
      return $ case cKind of
                  C.Cursor_FunctionDecl -> ChildVisit_Continue
                  C.Cursor_CXXMethod    -> ChildVisit_Continue
                  _                     -> ChildVisit_Recurse
-}

inProject :: WorkingDirectory -> SourceFile -> Bool
inProject wd p = (wd `T.isPrefixOf`) .&&. (not . T.null) $ p

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
    -- kind <- C.getKind cursor >>= C.getCursorKindSpelling >>= CS.unpack
    -- liftIO $ putStrLn $ "Cursor kind is " ++ kind
    defCursor <- C.getDefinition cursor
    isNullDef <- C.isNullCursor defCursor
    if isNullDef then do C.getReferenced cursor >>= reportIdentifier
                 else reportIdentifier defCursor
  where
    reportIdentifier cursor = do
      -- dumpSubtree cursor
      -- liftIO $ putStrLn $ "In file: " ++ (T.unpack f) ++ ":" ++ (show ln) ++ ":" ++ (show col) ++ " got name: " ++ name ++ " usr: " ++ usr
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
        liftIO $ putStrLn $ "Diagnostic: " ++ diaStr
  where
    isError = (== Diag.Diagnostic_Error) .||. (== Diag.Diagnostic_Fatal)


dumpSubtree :: C.Cursor -> ClangApp ()
dumpSubtree cursor = do
    dump 0 cursor
    void $ visitChildren cursor (dumpVisitor 0)
  where dumpVisitor :: Int -> ChildVisitor
        dumpVisitor i c _ = dump i c >> return ChildVisit_Recurse
        dump :: Int -> C.Cursor -> ClangApp ()
        dump i c = do
          -- Get extent.
          extent <- C.getExtent c
          (_, startLn, startCol, _) <- Source.getStart extent >>= Source.getSpellingLocation
          (_, endLn, endCol, _) <- Source.getEnd extent >>= Source.getSpellingLocation

          -- Get metadata.
          name <- C.getDisplayName cursor >>= CS.unpack
          usr <- XRef.getUSR cursor >>= CS.unpack
          kind <- C.getKind c >>= C.getCursorKindSpelling >>= CS.unpack

          -- Display.
          liftIO $ putStrLn $ (replicate i ' ') ++"[" ++ kind ++ "] " ++ name ++ " (" ++ usr ++ ") @ " ++
                              (show startLn) ++ "," ++ (show startCol) ++ " -> " ++
                              (show endLn) ++ "," ++ (show endCol)

withTranslationUnit :: CommandInfo -> (TranslationUnit -> ClangApp a) -> IO a
withTranslationUnit (CommandInfo sf _ (Command _ args) _) f = do
    withCreateIndex False False $ \index -> do
      withParse index (Just . unSourceFile $ sf) clangArgs [] [TranslationUnit_None] f bail
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
