{-# LANGUAGE DeriveDataTypeable #-}

module Pygmalion.Analyze.Source
( getIncludes -- deprecated
, getDefs     -- deprecated
, runSourceAnalyses
) where

import Clang.Alloc.Storable()
import qualified Clang.CrossReference as XRef
import qualified Clang.Cursor as C
import Clang.File as File
import Clang.Monad
import qualified Clang.Source as Source
import qualified Clang.String as CStr
import Clang.TranslationUnit
import Clang.Traversal
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.IORef
import Data.Typeable
import System.FilePath.Posix

import Data.Bool.Predicate
import Pygmalion.Analyze.Extension
import Pygmalion.Core

getIncludes :: CommandInfo -> IO (Maybe [FilePath])
getIncludes ci = runSourceAnalyses ci >>= return . (fst <$>)

getDefs :: CommandInfo -> IO (Maybe [DefInfo])
getDefs ci = runSourceAnalyses ci >>= return . (snd <$>)

runSourceAnalyses :: CommandInfo -> IO (Maybe ([FilePath], [DefInfo]))
runSourceAnalyses ci = do
  includesRef <- newIORef []
  defsRef <- newIORef []
  result <- try $ withTranslationUnit ci $ do
                    tu <- getTranslationUnit
                    includesAnalysis includesRef tu
                    defsAnalysis defsRef tu
                    return $ ()
  case result of
    Right _ -> (,) <$> readIORef includesRef <*> readIORef defsRef >>= return . Just
    Left (ClangException _) -> return Nothing

includesAnalysis :: IORef [FilePath] -> TranslationUnit -> ClangApp ()
includesAnalysis isRef tu = void $ getInclusions tu visitInclusions
  where
    visitInclusions :: InclusionVisitor
    visitInclusions file _  = do
      f <- File.getName file
      name <- CStr.unpack f
      when (isLocalHeader name) $ liftIO . modifyIORef isRef $! ((normalise name) :)

isLocalHeader :: FilePath -> Bool
isLocalHeader = isRelative .&&. isValid .&&. hasHeaderExtension .&&. (not . null)

defsAnalysis :: IORef [DefInfo] -> TranslationUnit -> ClangApp ()
defsAnalysis dsRef tu = do
    cursor <- getCursor tu
    void $ visitChildren cursor kidVisitor
  where
    kidVisitor :: ChildVisitor
    kidVisitor cursor _ = do
      cKind <- C.getKind cursor
      loc <- C.getLocation cursor
      (f, ln, col, _) <- Source.getSpellingLocation loc
      file <- case f of
                   Just validF -> File.getName validF >>= CStr.unpack
                   Nothing     -> return ""
      defined <- isDef cursor cKind
      when (inProject file && defined) $ do
        usr <- XRef.getUSR cursor >>= CStr.unpack
        name <- fqn cursor
        kind <- C.getCursorKindSpelling cKind >>= CStr.unpack
        -- liftIO $ putStrLn $ "Name: " ++ usr
        -- liftIO $ putStrLn $ "Kind: " ++ kind
        -- liftIO $ putStrLn $ "File: " ++ (normalise file)
        -- liftIO $ putStrLn $ "Line" ++ (show ln)
        -- liftIO $ putStrLn $ "Col" ++ (show col)
        -- liftIO $ putStrLn $ "Usr: " ++ usr
        def <- return $ DefInfo (Identifier name usr)
                          (SourceLocation (normalise file) ln col)
                          kind
        defEvaled <- liftIO . evaluate $ def
        liftIO . modifyIORef dsRef $! (defEvaled :)
      return $ case cKind of
                  C.Cursor_FunctionDecl -> ChildVisit_Continue
                  C.Cursor_CXXMethod    -> ChildVisit_Continue
                  _                     -> ChildVisit_Recurse

inProject :: FilePath -> Bool
inProject = isRelative .&&. isValid .&&. (not . null)

isDef :: C.Cursor -> C.CursorKind -> ClangApp Bool
isDef c k = do
  q1 <- C.isDefinition c
  return $ q1 && not (k == C.Cursor_CXXAccessSpecifier)

fqn :: C.Cursor -> ClangApp String
fqn cr = do
    result <- go cr
    liftM (intercalate "::" . reverse) (sequence result)
  where go c = do
          nc <- C.nullCursor
          isNull <- c `C.isSameCursor` nc
          if isNull then return []
            else do
              k <- C.getKind c
              isT <- C.isTranslationUnit k
              if isT then return []
                else do
                  parent <- C.getSemanticParent c
                  pl <- go parent
                  dn <- C.getDisplayName c
                  dns <- return $ CStr.unpack dn
                  return $ dns : pl

withTranslationUnit :: CommandInfo -> ClangApp () -> IO ()
withTranslationUnit (CommandInfo sf _ (Command _ args) _) f = do
    withCreateIndex False False $ \index -> do
      withParse index (Just sf) args [] [TranslationUnit_None] f bail
  where
    bail = throw . ClangException $ "Libclang couldn't parse " ++ sf

data ClangException = ClangException String
  deriving (Show, Typeable)
instance Exception ClangException
