import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.IO
import System.Process
import Test.Hspec
import Test.HUnit (assertBool)

import Pygmalion.Core

main :: IO ()
main = setCurrentDirectory "tests" >> runTests

runTests :: IO ()
runTests = do
  hspec $ before prepare $ do
  describe "pygclangindex" $ do
    it "indexes identifiers" $ do
      withPygd $ do
        index "identifiers.cpp"
        defs <- defsAt "identifiers.cpp" 4 10
        defs `shouldInclude` "3:7: Definition: main(int, char **)::identifier [VarDecl]"
        
prepare :: IO ()
prepare = do
  sh $ "rm -f " ++ dbFile

shouldInclude :: [String] -> String -> Expectation
shouldInclude ss s = assertBool errorMsg $ any (s `isInfixOf`) ss
  where
    errorMsg = show ss ++ " doesn't include " ++ show s

withPygd :: IO () -> IO ()
withPygd action = bracket startPygd stopPygd (\_ -> action)
  where
    startPygd = do bg $ "../dist/build/pygd/pygd"
                   threadDelay 1000000
    stopPygd _ = void $ pygmalion ["--stop"]
  
pygmalion :: [String] -> IO [String]
pygmalion args = do
  let cmd = proc "../dist/build/pygmalion/pygmalion" args
  (_, Just out, _, h) <- createProcess $ cmd { std_out = CreatePipe }
  output <- hGetContents out
  waitForProcess h
  return (lines output)

index :: FilePath -> IO ()
index file = do
  void $ pygmalion ["--index", "clang++", file]
  threadDelay 1000000

defsAt :: FilePath -> Int -> Int -> IO [String]
defsAt file line col = pygmalion ["--definition", file, show line, show col]

sh :: String -> IO ()
sh cmd = void $ waitForProcess =<< runCommand cmd

bg :: String -> IO ()
bg cmd = void $ runCommand cmd
