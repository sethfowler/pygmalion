{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Pygmalion.Analyze.Source
( runSourceAnalyses
, getLookupInfo
, LookupInfo (..)
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
import Data.IORef
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Data.Typeable
import Data.Word

import Data.Bool.Predicate
import Pygmalion.Analyze.Extension
import Pygmalion.Core

runSourceAnalyses :: T.Text -> CommandInfo -> IO (Maybe ([SourceFile], [DefInfo]))
runSourceAnalyses wd ci = do
  includesRef <- newIORef $! []
  defsRef <- newIORef $! []
  result <- try $ withTranslationUnit ci $ do
                    includesAnalysis includesRef wd
                    defsAnalysis defsRef wd
  case result of
    Right _ -> (,) <$> readIORef includesRef <*> readIORef defsRef >>= return . Just
    Left (ClangException e) -> putStrLn ("Clang exception: " ++ e) >> return Nothing

data LookupInfo = GotDef DefInfo
                | GotUSR USR
                | GotNothing
                deriving (Eq, Show)

getLookupInfo :: CommandInfo -> SourceLocation -> IO LookupInfo
getLookupInfo ci sl = do
  result <- try $ withTranslationUnit ci $ inspectIdentifier sl
  case result of
    Right r                 -> return r
    Left (ClangException e) -> putStrLn ("Clang exception: " ++ e ) >> return GotNothing

includesAnalysis :: IORef [SourceFile] -> T.Text -> ClangApp ()
includesAnalysis isRef wd = do
    tu <- getTranslationUnit
    void $ getInclusions tu visitInclusions
  where
    visitInclusions :: InclusionVisitor
    visitInclusions file _  = do
      f <- File.getName file
      name <- CS.unpackText f
      when (isLocalHeader wd name) $
        liftIO . modifyIORef' isRef $! (name :)

isLocalHeader :: T.Text -> T.Text -> Bool
isLocalHeader wd p = (wd `T.isPrefixOf`) .&&.
                     hasHeaderExtensionText .&&.
                     (not . T.null) $ p

defsAnalysis :: IORef [DefInfo] -> T.Text -> ClangApp ()
defsAnalysis dsRef wd = do
    tu <- getTranslationUnit
    cursor <- getCursor tu
    fileCache <- liftIO $ mkCache CS.unpackText T.unpack
    void $ visitChildren cursor (kidVisitor fileCache)
  where
    kidVisitor :: StringCache T.Text -> ChildVisitor
    kidVisitor fileCache cursor _ = do
      loc <- C.getLocation cursor
      (f, ln, col, _) <- Source.getSpellingLocation loc
      file <- case f of Just valid -> File.getName valid >>= fromCache fileCache
                        Nothing    -> return ""
      case (inProject wd file) of
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
  -- liftIO $ putStrLn $ "Got hash " ++ (show hash) ++ " for name " ++ name
  cacheMap <- liftIO $ readIORef (cacheHashMap cache)
  case hash `Map.lookup` cacheMap of
    Just s -> do --liftIO $ putStrLn $ "Cache HIT for " ++ (cachedAsString cache s)
                 return $! s
    Nothing -> do s <- (stringPreparer cache) cxStr
                  --liftIO $ putStrLn $ "Cache MISS for " ++ (cachedAsString cache s)
                  liftIO $ modifyIORef' (cacheHashMap cache) $! (Map.insert hash s)
                  return $! s

inProject :: T.Text -> T.Text -> Bool
inProject wd p = (wd `T.isPrefixOf`) .&&. (not . T.null) $ p

isDef :: C.Cursor -> C.CursorKind -> ClangApp Bool
isDef c k = do
  q1 <- C.isDefinition c
  return $ q1 && not (k == C.Cursor_CXXAccessSpecifier)

fqn :: C.Cursor -> ClangApp T.Text
fqn cursor = (T.intercalate "::" . reverse) <$> go cursor
  where go c = do isNull <- C.isNullCursor c
                  isTU <- C.getKind c >>= C.isTranslationUnit
                  if isNull || isTU then return [] else go' c
        go' c =  (:) <$> (cursorName c) <*> (C.getSemanticParent c >>= go)

cursorName :: C.Cursor -> ClangApp T.Text
cursorName c = C.getDisplayName c >>= CS.unpackText >>= anonymize
  where anonymize s | T.null s  = return "<anonymous>"
                    | otherwise = return s

inspectIdentifier :: SourceLocation -> ClangApp LookupInfo
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
      -- liftIO $ putStrLn $ "In file: " ++ (T.unpack f) ++ ":" ++ (show ln) ++ ":" ++ (show col) ++ " got name: " ++ name ++ " usr: " ++ usr
      isNull <- C.isNullCursor cursor
      case isNull of
        False -> do usr <- XRef.getUSR cursor >>= CS.unpackText
                    kind <- C.getKind cursor 
                    cursorIsDef <- isDef cursor kind
                    if cursorIsDef then reportDef cursor usr kind
                                   else return (GotUSR usr)
        True -> return GotNothing
    reportDef cursor usr k = do
      name <- C.getDisplayName cursor >>= CS.unpackText
      kind <- C.getCursorKindSpelling k >>= CS.unpackText
      loc <- C.getLocation cursor
      (df, dl, dc, _) <- Source.getSpellingLocation loc
      file <- case df of Just valid -> File.getName valid >>= CS.unpackText
                         Nothing    -> return ""
      return $ if (not $ T.null file) then GotDef (DefInfo name usr (SourceLocation file dl dc) kind)
                                      else GotUSR usr

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
