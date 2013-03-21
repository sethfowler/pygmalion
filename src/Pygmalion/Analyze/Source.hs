{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Pygmalion.Analyze.Source
( getIncludes
, getDefs
) where

import Clang.Alloc.Storable()
--import qualified Clang.CrossReference as XRef
import qualified Clang.Cursor as Cursor
import Clang.File
import Clang.FFI (TranslationUnit)
import qualified Clang.Source as Source
import Clang.TranslationUnit
import Clang.Traversal
import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.List
import Data.IORef
import Data.Typeable
--import Foreign.StablePtr
--import System.IO
import System.FilePath.Posix

import Pygmalion.Core

getIncludes :: CommandInfo -> IO (Maybe [FilePath])
getIncludes = runAnalysis includesAnalysis ([] :: [FilePath])

getDefs :: CommandInfo -> IO (Maybe [String])
getDefs = runAnalysis defsAnalysis ([] :: [String])

type Analysis a = IORef a -> TranslationUnit -> IO ()
runAnalysis :: NFData a => Analysis a -> a -> CommandInfo -> IO (Maybe a)
runAnalysis f st ci = do
  stRef <- newIORef (force st)
  result <- try $ withTranslationUnit ci (f stRef)
  case result of
    Right _                 -> readIORef stRef >>= return . Just . force
    Left (ClangException _) -> return Nothing

includesAnalysis :: IORef [FilePath] -> TranslationUnit -> IO ()
includesAnalysis hsRef tu = void $ getInclusions tu visitInclusions unused
  where
    visitInclusions :: InclusionVisitor Bool
    visitInclusions file _ usrData = do
      let name = getName file
      case null name of
        True  -> putStrLn "Got null filename"
        False -> modifyIORef hsRef $ \hs -> force (name : hs)
      return usrData

defsAnalysis :: IORef [String] -> TranslationUnit -> IO ()
defsAnalysis _ tu = void $ visitChildren (getCursor tu) kidVisitor unused
  where
    kidVisitor :: ChildVisitor Bool
    kidVisitor cursor _ usrData = do
      let cKind = Cursor.getKind cursor
      let kind = show (Cursor.getCursorKindSpelling cKind)
      let loc = Cursor.getLocation cursor
      let (f, ln, col, _) = Source.getSpellingLocation loc
      let file = case f of
                   Nothing -> ""
                   Just validF  -> show $ Source.getFilename validF
      when (inProject file && isDef cursor cKind) $
        putStrLn $ "Name: " ++ (fqn cursor) ++ " Kind: " ++ kind ++ " Loc: " ++
                   (normalise file) ++ ":" ++ (show ln) ++ ":" ++ (show col)
      let next = case cKind of
                  Cursor.Cursor_FunctionDecl -> ChildVisit_Continue
                  Cursor.Cursor_CXXMethod    -> ChildVisit_Continue
                  _                          -> ChildVisit_Recurse
      return (usrData, next)

inProject :: String -> Bool
inProject file = not (("/" `isPrefixOf` file) || (null file)) 

isDef :: Cursor.Cursor -> Cursor.CursorKind -> Bool
isDef c k = Cursor.isDefinition c && not (k == Cursor.Cursor_CXXAccessSpecifier)

fqn :: Cursor.Cursor -> String
fqn = intercalate "::" . reverse . go
  where go c | c == Cursor.nullCursor = []
             | (Cursor.isTranslationUnit . Cursor.getKind) c = []
             | otherwise = show (Cursor.getDisplayName c) : go (Cursor.getSemanticParent c)

{-
withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr v = bracket (newStablePtr v) (freeStablePtr)
-}

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
