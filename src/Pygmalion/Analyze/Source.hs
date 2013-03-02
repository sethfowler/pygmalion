module Pygmalion.Analyze.Source
( clangGetIncludes
) where

import Clang.Alloc.Storable()
import Clang.File
import Clang.FFI
import Clang.TranslationUnit
import Control.Exception
import Data.IORef
import Data.Maybe
import Foreign.StablePtr

import Pygmalion.Core

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr v = bracket (newStablePtr v) (freeStablePtr)

clangAnalyze :: CommandInfo -> (TranslationUnit -> IO ()) -> IO ()
clangAnalyze (CommandInfo sf _ [] _) _ = error $ "No command for " ++ sf
clangAnalyze (CommandInfo sf _ (_ : args) _) f = do
    withCreateIndex False False $ \index -> do
      withParse index (Just sf) args [] [TranslationUnit_None] f bail
  where
    bail = error $ "Couldn't parse " ++ sf

clangGetIncludes :: CommandInfo -> IO [FilePath]
clangGetIncludes ci = do
    headersRef <- newIORef []
    clangAnalyze ci (getHeaders headersRef)
    readIORef headersRef
  where
    getHeaders hsRef tu = withStablePtr hsRef $ \hsRefPtr ->
      getInclusions tu visitInclusions (Just hsRefPtr)
    visitInclusions :: InclusionVisitor (StablePtr (IORef [String]))
    visitInclusions file _ hsRefPtr = do
      let name = getName file
      case null name of
        True -> putStrLn "Got null filename"
        False -> do
          hsRef <- deRefStablePtr . fromJust $ hsRefPtr
          modifyIORef hsRef $ \hs -> name : hs
      return hsRefPtr
