{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Pygmalion.Analyze.Source
( runSourceAnalyses
, getIdentifier
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
import Data.List
import Data.IORef
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Data.Typeable
import Data.Word
import System.Directory

import Data.Bool.Predicate
import Pygmalion.Analyze.Extension
import Pygmalion.Core

runSourceAnalyses :: CommandInfo -> IO (Maybe ([SourceFile], [DefInfo]))
runSourceAnalyses ci = do
  wd <- getCurrentDirectory -- FIXME: Should really pass this in.
  includesRef <- newIORef $! []
  defsRef <- newIORef $! []
  result <- try $ withTranslationUnit ci $ do
                    includesAnalysis includesRef wd
                    defsAnalysis defsRef wd
  case result of
    Right _ -> (,) <$> readIORef includesRef <*> readIORef defsRef >>= return . Just
    Left (ClangException e) -> putStrLn ("Clang exception: " ++ e) >> return Nothing

getIdentifier :: CommandInfo -> SourceLocation -> IO (Maybe Identifier)
getIdentifier ci sl = do
  result <- try $ withTranslationUnit ci $ inspectIdentifier sl
  case result of
    Right identifier        -> return identifier
    Left (ClangException _) -> return Nothing

includesAnalysis :: IORef [SourceFile] -> FilePath -> ClangApp ()
includesAnalysis isRef wd = do
    tu <- getTranslationUnit
    void $ getInclusions tu visitInclusions
  where
    visitInclusions :: InclusionVisitor
    visitInclusions file _  = do
      f <- File.getName file
      name <- CS.unpack f
      when (isLocalHeader wd name) $
        liftIO . modifyIORef' isRef $! ((mkSourceFile name) :)

isLocalHeader :: FilePath -> FilePath -> Bool
isLocalHeader wd p = (wd `isPrefixOf`) .&&. hasHeaderExtension .&&. (not . null) $ p

defsAnalysis :: IORef [DefInfo] -> FilePath -> ClangApp ()
defsAnalysis dsRef wd = do
    tu <- getTranslationUnit
    cursor <- getCursor tu
    fileCache <- liftIO $ mkCache CS.unpack id
    attrCache <- liftIO $ mkCache CS.unpackText T.unpack
    void $ visitChildren cursor (kidVisitor fileCache attrCache)
  where
    kidVisitor :: StringCache FilePath -> StringCache T.Text -> ChildVisitor
    kidVisitor fileCache attrCache cursor _ = do
      loc <- C.getLocation cursor
      (f, ln, col, _) <- Source.getSpellingLocation loc
      file <- case f of Just valid -> File.getName valid >>= fromCache fileCache
                        Nothing    -> return ""
      when (inProject wd file) $ do
        cKind <- C.getKind cursor
        defined <- isDef cursor cKind
        when defined $ do
          usr <- XRef.getUSR cursor >>= fromCache attrCache
          name <- fqn attrCache cursor
          kind <- C.getCursorKindSpelling cKind >>= fromCache attrCache
          def <- return $! DefInfo (Identifier name usr)
                            (SourceLocation (mkSourceFile file) ln col)
                            kind
          liftIO . modifyIORef' dsRef $! (def :)
      return $ if (inProject wd file) then ChildVisit_Recurse
                                      else ChildVisit_Continue

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

-- This whole thing needs cleanup; it serves only as a proof of concept.
data StringCache a = StringCache {
                        cacheHashMap :: IORef (Map.HashMap Word64 a),
                        stringPreparer :: CS.ClangString -> ClangApp a,
                        cachedAsString :: a -> String
                       }

mkCache :: (CS.ClangString -> ClangApp a) -> (a -> String) -> IO (StringCache a)
mkCache prep asString = do
  cacheRef <- newIORef $! Map.empty
  return $ StringCache cacheRef prep asString

fromCache :: StringCache a -> CS.ClangString -> ClangApp a
fromCache cache cxStr = do
  hash <- CS.hash cxStr
  cacheMap <- liftIO $ readIORef (cacheHashMap cache)
  case hash `Map.lookup` cacheMap of
    Just s -> do --liftIO $ putStrLn $ "Cache HIT for " ++ (cachedAsString cache s)
                 return $! s
    Nothing -> do s <- (stringPreparer cache) cxStr
                  --liftIO $ putStrLn $ "Cache MISS for " ++ (cachedAsString cache s)
                  liftIO $ modifyIORef' (cacheHashMap cache) $! (Map.insert hash s)
                  return $! s

inProject :: FilePath -> FilePath -> Bool
inProject wd p = (wd `isPrefixOf`) .&&. (not . null) $ p

isDef :: C.Cursor -> C.CursorKind -> ClangApp Bool
isDef c k = do
  q1 <- C.isDefinition c
  return $ q1 && not (k == C.Cursor_CXXAccessSpecifier)

fqn :: StringCache T.Text -> C.Cursor -> ClangApp T.Text
fqn cache cursor = (T.intercalate "::" . reverse) <$> go cursor
  where go c = do isNull <- C.isNullCursor c
                  isTU <- C.getKind c >>= C.isTranslationUnit
                  if isNull || isTU then return [] else go' c
        go' c =  (:) <$> (cursorName cache c) <*> (C.getSemanticParent c >>= go)

cursorName :: StringCache T.Text -> C.Cursor -> ClangApp T.Text
cursorName cache c = C.getDisplayName c >>= fromCache cache >>= anonymize
  where anonymize s | T.null s  = return "<anonymous>"
                    | otherwise = return s

inspectIdentifier :: SourceLocation -> ClangApp (Maybe Identifier)
inspectIdentifier (SourceLocation f ln col) = do
    dumpDiagnostics
    tu <- getTranslationUnit
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
      name <- C.getDisplayName cursor >>= CS.unpackText
      usr <- XRef.getUSR cursor >>= CS.unpackText
      -- liftIO $ putStrLn $ "In file: " ++ (T.unpack f) ++ ":" ++ (show ln) ++ ":" ++ (show col) ++ " got name: " ++ name ++ " usr: " ++ usr
      return $ if T.null usr then Nothing
                             else Just $ Identifier name usr

-- We need to decide on a policy, but it'd be good to figure out a way to let
-- the user display these, and maybe always display errors.
dumpDiagnostics :: ClangApp ()
dumpDiagnostics = do
    tu <- getTranslationUnit
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

withTranslationUnit :: CommandInfo -> ClangApp a -> IO a
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
