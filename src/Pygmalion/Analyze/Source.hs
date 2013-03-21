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
import Clang.FFI (TranslationUnit)
import qualified Clang.Source as Source
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
  case result of
    Right _ -> (,) <$> readIORef includesRef <*> readIORef defsRef >>= return . Just
    Left (ClangException _) -> return Nothing

includesAnalysis :: IORef [FilePath] -> TranslationUnit -> IO ()
includesAnalysis isRef tu = void $ getInclusions tu visitInclusions unused
  where
    visitInclusions :: InclusionVisitor Bool
    visitInclusions file _ usrData = do
      let name = getName file
      when (isLocalHeader name) $ modifyIORef isRef $! ((normalise name) :)
      return usrData

isLocalHeader :: FilePath -> Bool
isLocalHeader = isRelative .&&. isValid .&&. hasHeaderExtension .&&. (not . null)

defsAnalysis :: IORef [DefInfo] -> TranslationUnit -> IO ()
defsAnalysis dsRef tu = void $ visitChildren (getCursor tu) kidVisitor unused
  where
    kidVisitor :: ChildVisitor Bool
    kidVisitor cursor _ usrData = do
      let cKind = C.getKind cursor
      let loc = C.getLocation cursor
      let (f, ln, col, _) = Source.getSpellingLocation loc
      let file = case f of
                   Just validF -> show $ Source.getFilename validF
                   Nothing     -> ""
      when (inProject file && isDef cursor cKind) $ do
        let usr = show $ XRef.getUSR cursor
        let name = fqn cursor
        let kind = show (C.getCursorKindSpelling cKind)
        putStrLn $ "Name: " ++ name ++ " Kind: " ++ kind ++ " Loc: " ++
                   (normalise file) ++ ":" ++ (show ln) ++ ":" ++ (show col)
                   ++ " Usr: " ++ usr
        let def = DefInfo (Identifier name usr)
                          (SourceLocation (normalise file) ln col)
                          kind
        modifyIORef dsRef $! (def :)
      let next = case cKind of
                  C.Cursor_FunctionDecl -> ChildVisit_Continue
                  C.Cursor_CXXMethod    -> ChildVisit_Continue
                  _                     -> ChildVisit_Recurse
      return (usrData, next)

inProject :: FilePath -> Bool
inProject = isRelative .&&. isValid .&&. (not . null)

isDef :: C.Cursor -> C.CursorKind -> Bool
isDef c k = C.isDefinition c && not (k == C.Cursor_CXXAccessSpecifier)

fqn :: C.Cursor -> String
fqn = intercalate "::" . reverse . go
  where go c | c == C.nullCursor = []
             | (C.isTranslationUnit . C.getKind) c = []
             | otherwise = show (C.getDisplayName c) : go (C.getSemanticParent c)

withTranslationUnit :: CommandInfo -> (TranslationUnit -> IO ()) -> IO ()
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
