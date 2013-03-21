{-# LANGUAGE DeriveDataTypeable #-}

module Pygmalion.Analyze.Source
( getIncludes -- deprecated
, getDefs     -- deprecated
, runSourceAnalyses
) where

import Clang.Alloc.Storable()
import qualified Clang.CrossReference as XRef
import qualified Clang.Cursor as C
import Clang.File
import qualified Clang.FFI as FFI
--import qualified Clang.Source as Source
import Clang.TranslationUnit
import Clang.Traversal
import Control.Applicative
import Control.Exception
import Control.Monad
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
  result <- try $ withTranslationUnit ci $ \tu -> do
                    includesAnalysis includesRef tu
                    defsAnalysis defsRef tu
                    return $ ()
  case result of
    Right _ -> (,) <$> readIORef includesRef <*> readIORef defsRef >>= return . Just
    Left (ClangException _) -> return Nothing

includesAnalysis :: IORef [FilePath] -> FFI.TranslationUnit -> IO ()
includesAnalysis isRef tu = void $ getInclusions tu visitInclusions unused
  where
    visitInclusions :: InclusionVisitor Bool
    visitInclusions file _ usrData = do
      f <- FFI.getFileName file
      name <- evaluate $ show f
      when (isLocalHeader name) $ modifyIORef isRef $! ((normalise name) :)
      return usrData

isLocalHeader :: FilePath -> Bool
isLocalHeader = isRelative .&&. isValid .&&. hasHeaderExtension .&&. (not . null)

defsAnalysis :: IORef [DefInfo] -> FFI.TranslationUnit -> IO ()
defsAnalysis dsRef tu = void $ visitChildren (getCursor tu) kidVisitor unused
  where
    kidVisitor :: ChildVisitor Bool
    kidVisitor cursor _ usrData = do
      cKind <- FFI.getCursorKind cursor
      loc <- FFI.getCursorLocation cursor
      (f, ln, col, _) <- FFI.getSpellingLocation loc
      file <- case f of
                   Just validF -> FFI.getFileName validF >>= return . show
                   Nothing     -> return ""
      --when (inProject file && isDef cursor cKind) $ do
      --isItDef <- isDef cursor cKind
      let isItDef = True
      when (inProject file && isItDef) $ do
        usr <- FFI.getCursorUSR cursor >>= return . show
        name <- fqn cursor
        kind <- FFI.getCursorKindSpelling cKind >>= return . show
        -- putStrLn $ "Name: " ++ usr
        -- putStrLn $ "Kind: " ++ kind
        -- putStrLn $ "File: " ++ (normalise file)
        -- putStrLn $ "Line" ++ (show ln)
        -- putStrLn $ "Col" ++ (show col)
        -- putStrLn $ "Usr: " ++ usr
        def <- return $ DefInfo (Identifier name usr)
                          (SourceLocation (normalise file) ln col)
                          kind
        defEvaled <- evaluate def
        modifyIORef dsRef $! (defEvaled :)
      let next = case cKind of
                  C.Cursor_FunctionDecl -> ChildVisit_Continue
                  C.Cursor_CXXMethod    -> ChildVisit_Continue
                  _                     -> ChildVisit_Recurse
      return (usrData, next)

inProject :: FilePath -> Bool
inProject = isRelative .&&. isValid .&&. (not . null)

isDef :: C.Cursor -> C.CursorKind -> IO Bool
isDef c k = do
  q1 <- FFI.isCursorDefinition c
  return $ q1 && not (k == C.Cursor_CXXAccessSpecifier)

fqn :: C.Cursor -> IO String
fqn cr = do
    final <- go cr
    return $ intercalate "::" . reverse $ final
  where go c = do
          nc <- FFI.getNullCursor
          if c == nc then return []
            else do
              k <- FFI.getCursorKind c
              isT <- FFI.isTranslationUnit k
              if isT then return []
                else do
                  parent <- FFI.getCursorSemanticParent c
                  pl <- go parent
                  dn <- FFI.getCursorDisplayName c
                  dns <- return $ show dn
                  return $ dns : pl

withTranslationUnit :: CommandInfo -> (FFI.TranslationUnit -> IO ()) -> IO ()
withTranslationUnit (CommandInfo sf _ (Command _ args) _) f = do
    withCreateIndex False False $ \index -> do
      withParse index (Just sf) args [] [TranslationUnit_None] f bail
  where
    bail = throw . ClangException $ "Libclang couldn't parse " ++ sf

unused :: Maybe Bool
unused = Just True

data ClangException = ClangException String
  deriving (Show, Typeable)
instance Exception ClangException
