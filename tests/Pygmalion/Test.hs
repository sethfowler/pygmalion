{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

module Pygmalion.Test
( defShouldBe
, defsShouldBe
, index
, line
, PygmalionTest (..)
, runPygmalionTest
, Test (..)
, test
, withPygd
) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (forM_, void)
import Control.Monad.State (execState, get, modify, State)
import Control.Monad.State.Class (MonadState)
import Data.List (isInfixOf)
import Data.Tuple.Curry (uncurryN)
import System.Directory (removeFile)
import System.IO (hGetContents, hPutStr, withFile, IOMode(..))
import System.Process (CreateProcess(..), createProcess, runCommand, proc, StdStream(..), waitForProcess)
import Test.Hspec (Expectation, pendingWith)
import Test.HUnit (assertBool)

import Pygmalion.Core (dbFile)

type Line = Int
type Col = Int

data Test = Def String
          | Defs [String]
          | Fails String
            deriving (Eq, Show)

data AnnotatedTest = DefTest Line Col [String]
                   | FailingTest String
                     deriving (Eq, Show)

data TestDesc = TestDesc
  { tsLines :: [String]
  , tsTests :: [AnnotatedTest]
  } deriving (Eq, Show)

addDescLine :: String -> TestDesc -> TestDesc
addDescLine s d = d { tsLines = (tsLines d) ++ [s] }

addDescTest :: AnnotatedTest -> TestDesc -> TestDesc
addDescTest t d = d { tsTests = (tsTests d) ++ [t] }

newtype PygmalionTest a = PygmalionTest
  { unPygmalionTest :: State TestDesc a
  } deriving (Functor, Monad, MonadState TestDesc)

line :: String -> PygmalionTest ()
line s = modify (addDescLine s)

test :: String -> [Test] -> PygmalionTest ()
test s ts = do
  l <- (+1) . length . tsLines <$> get
  let (finalLine, finalTests) = annotateTests l 1 s ts
  modify (addDescLine finalLine)
  forM_ finalTests $ \t ->
    modify (addDescTest t)

annotateTests :: Line -> Col -> String -> [Test] -> (String, [AnnotatedTest])
annotateTests _ _ [] [] = ([], [])
annotateTests _ _ [] (x:_) = error $ "Test " ++ (show x) ++ " doesn't have a location"
annotateTests l c ('@':_) [] = error $ "Location " ++ (show (l,c)) ++ " doesn't have a test"
annotateTests l c ('@':xs) (t:ts) = let (xs', ts') = annotateTests l (c + 1) xs ts in
                                    (xs', (annotateTest l c t) : ts')
annotateTests l c (x:xs) ts = let (xs', ts') = annotateTests l (c + 1) xs ts in
                              ((x:xs'), ts')

annotateTest :: Line -> Col -> Test -> AnnotatedTest
annotateTest l c (Def s)   = DefTest l c [s]
annotateTest l c (Defs ss) = DefTest l c ss
annotateTest l c (Fails s) = FailingTest $ (show (l,c)) ++ ": " ++ s

runPygmalionTest :: FilePath -> PygmalionTest () -> Expectation
runPygmalionTest path t = finally go cleanup
  where
    cleanup = removeFile path  -- Remove the temporary file no matter what.
    go = do
      let desc = execState (unPygmalionTest t) (TestDesc [] [])

      -- Write test to a temporary file.
      withFile path WriteMode $ \h ->
        hPutStr h (unlines . tsLines $ desc)

      -- Run the indexer.
      index path

      -- Run the tests.
      forM_ (tsTests desc) $ \case
        DefTest ln col ss -> (path, ln, col) `defsShouldBe` ss
        FailingTest s     -> pendingWith s
             
defShouldBe :: (FilePath, Line, Col) -> String -> Expectation
defShouldBe loc s = do
    ds <- uncurryN defsAt loc
    assertBool (errorMsg ds) $ length ds == 1
    assertBool (errorMsg ds) $ any (s `isInfixOf`) ds
  where
    errorMsg ds = "Definition for " ++ show loc ++ " was " ++ show ds ++ "; expected " ++ show s

defsShouldBe :: (FilePath, Line, Col) -> [String] -> Expectation
defsShouldBe loc ss = do
    ds <- uncurryN defsAt loc
    assertBool (errorMsg ds) $ length ds == length ss
    forM_ ss $ \s ->
      assertBool (errorMsg ds) $ any (s `isInfixOf`) ds
  where
    errorMsg ds = "Definition for " ++ show loc ++ " was " ++ show ds ++ "; expected " ++ show ss

withPygd :: IO () -> IO ()
withPygd action = bracket startPygd stopPygd (const action)
  where
    startPygd = do bg "../dist/build/pygd/pygd"
                   threadDelay 100000
    stopPygd _ = do void $ pygmalion ["stop-server"]
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
  void $ pygmalion ["wait"]

defsAt :: FilePath -> Line -> Col -> IO [String]
defsAt file l c = pygmalion ["definition", file, show l, show c]

sh :: String -> IO ()
sh cmd = void $ waitForProcess =<< runCommand cmd

bg :: String -> IO ()
bg cmd = void $ runCommand cmd
