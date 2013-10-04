module Pygmalion.Test
( defShouldBe
, defsShouldBe
, index
, withPygd
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.List (isInfixOf)
import Data.Tuple.Curry (uncurryN)
import System.IO (hGetContents)
import System.Process (CreateProcess(..), createProcess, runCommand, proc, StdStream(..), waitForProcess)
import Test.Hspec (Expectation)
import Test.HUnit (assertBool)

import Pygmalion.Core (dbFile)

defShouldBe :: (FilePath, Int, Int) -> String -> Expectation
defShouldBe loc s = do
    ds <- uncurryN defsAt loc
    assertBool (errorMsg ds) $ length ds == 1
    assertBool (errorMsg ds) $ any (s `isInfixOf`) ds
  where
    errorMsg ds = "Definition for " ++ show loc ++ " was " ++ show ds ++ "; expected " ++ show s

defsShouldBe :: (FilePath, Int, Int) -> [String] -> Expectation
defsShouldBe loc ss = do
    ds <- uncurryN defsAt loc
    forM_ ss $ \s ->
      assertBool (errorMsg ds) $ any (s `isInfixOf`) ds
  where
    errorMsg ds = "Definition for " ++ show loc ++ " was " ++ show ds ++ "; expected " ++ show ss

withPygd :: IO () -> IO ()
withPygd action = bracket startPygd stopPygd (const action)
  where
    startPygd = do bg "../dist/build/pygd/pygd"
                   threadDelay 1000000
    stopPygd _ = do void $ pygmalion ["stop"]
                    sh $ "rm -f " ++ dbFile
  
pygmalion :: [String] -> IO [String]
pygmalion args = do
  let cmd = proc "../dist/build/pygmalion/pygmalion" args
  (_, Just out, _, h) <- createProcess $ cmd { std_out = CreatePipe }
  output <- hGetContents out
  _ <- waitForProcess h
  return (lines output)

index :: FilePath -> IO ()
index file = do
  void $ pygmalion ["index", "clang++", "--std=c++11", file]
  threadDelay 1000000

defsAt :: FilePath -> Int -> Int -> IO [String]
defsAt file line col = pygmalion ["definition", file, show line, show col]

sh :: String -> IO ()
sh cmd = void $ waitForProcess =<< runCommand cmd

bg :: String -> IO ()
bg cmd = void $ runCommand cmd
