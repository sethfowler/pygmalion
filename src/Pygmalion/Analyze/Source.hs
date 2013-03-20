{-# LANGUAGE DeriveDataTypeable #-}

module Pygmalion.Analyze.Source
( clangGetIncludes
) where

import Clang.Alloc.Storable()
import qualified Clang.Cursor as Cursor
import Clang.File
import Clang.FFI (TranslationUnit)
import Clang.TranslationUnit
import Clang.Traversal
import Control.DeepSeq
import Control.Exception
import Data.IORef
import Data.Typeable
import Foreign.StablePtr

import Pygmalion.Core

data ClangException = ClangException String
  deriving (Show, Typeable)
instance Exception ClangException

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr v = bracket (newStablePtr v) (freeStablePtr)

clangAnalyze :: CommandInfo -> (TranslationUnit -> IO ()) -> IO ()
clangAnalyze (CommandInfo sf _ (Command _ args) _) f = do
    withCreateIndex False False $ \index -> do
      withParse index (Just sf) args [] [TranslationUnit_None] f bail
  where
    bail = throw . ClangException $ "Libclang couldn't parse " ++ sf

clangGetIncludes :: CommandInfo -> IO (Maybe [FilePath])
clangGetIncludes ci = do
    headersRef <- newIORef (force [])
    result <- try $ clangAnalyze ci (\tu -> getHeaders headersRef tu >> visitKids tu)
    case result of
      Right _                 -> readIORef headersRef >>= return . Just . force
      Left (ClangException _) -> return Nothing
  where
    visitKids tu = visitChildren (getCursor tu) kidVisitor Nothing >> return ()
    kidVisitor :: ChildVisitor Bool
    kidVisitor cursor _ usrData = do
      let cKind = Cursor.getKind cursor
      let nameString = show (Cursor.getDisplayName cursor)
      let kindString = show (Cursor.getCursorKindSpelling cKind)
      putStrLn $ "Name: " ++ nameString ++ " Kind: " ++ kindString
      return (usrData, ChildVisit_Continue)
    getHeaders hsRef tu = withStablePtr hsRef $ \hsRefPtr ->
      getInclusions tu (visitInclusions hsRefPtr) (Just True)
    visitInclusions :: StablePtr (IORef [String]) -> InclusionVisitor Bool
    visitInclusions hsRefPtr file _ _ = do
      let name = getName file
      case null name of
        True -> putStrLn "Got null filename"
        False -> do
          hsRef <- deRefStablePtr hsRefPtr
          modifyIORef hsRef $ \hs -> force (name : hs)
      return $ Just True
