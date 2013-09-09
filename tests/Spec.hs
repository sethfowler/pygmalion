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
  describe "pygindex-clang" $ do
    it "indexes local variables" $ do
      withPygd $ do
        index "local-variables.cpp"
        defs <- defsAt "local-variables.cpp" 4 10
        defs `shouldInclude` "3:7: Definition: main(int, char **)::var [VarDecl]"
    it "indexes global variables" $ do
      withPygd $ do
        index "global-variables.cpp"
        defs <- defsAt "global-variables.cpp" 5 10
        defs `shouldInclude` "1:12: Definition: var [VarDecl]"
    it "indexes preprocessor macros" $ do
      withPygd $ do
        index "preprocessor-macros.cpp"
        defs <- defsAt "preprocessor-macros.cpp" 5 10
        defs `shouldInclude` "1:9: Definition: VAR [MacroDefinition]"
    it "indexes preprocessor macros" $ do
      withPygd $ do
        index "preprocessor-functions.cpp"
        defs <- defsAt "preprocessor-functions.cpp" 5 10
        defs `shouldInclude` "1:9: Definition: VAR [MacroDefinition]"
    it "indexes global functions" $ do
      withPygd $ do
        index "functions.cpp"
        defs <- defsAt "functions.cpp" 5 10
        defs `shouldInclude` "1:5: Definition: var() [FunctionDecl]"
    it "indexes enums" $ do
      withPygd $ do
        index "enums.cpp"
        defsA <- defsAt "enums.cpp" 8 10
        defsA `shouldInclude` "1:20: Definition: global_enum::global_enum_val [EnumConstantDecl]"
        defsB <- defsAt "enums.cpp" 9 10
        defsB `shouldInclude` "2:8: Definition: <anonymous>::global_anonymous_enum_val [EnumConstantDecl]"
        defsC <- defsAt "enums.cpp" 10 10
        defsC `shouldInclude` "6:21: Definition: main(int, char **)::local_enum::local_enum_val [EnumConstantDecl]"
        defsD <- defsAt "enums.cpp" 11 10
        defsD `shouldInclude` "7:10: Definition: main(int, char **)::<anonymous>::local_anonymous_enum_val [EnumConstantDecl]"
    it "indexes structs" $ do
      withPygd $ do
        index "structs.cpp"
        defsA <- defsAt "structs.cpp" 12 10
        defsA `shouldInclude` "9:17: Definition: main(int, char **)::global_struct_var [VarDecl]"
        defsB <- defsAt "structs.cpp" 12 28 
        defsB `shouldInclude` "1:28: Definition: global_struct::global_struct_val [FieldDecl]"
        defsC <- defsAt "structs.cpp" 13 10
        defsC `shouldInclude` "2:45: Definition: global_anonymous_struct_var [VarDecl]"
        defsD <- defsAt "structs.cpp" 13 38
        defsD `shouldInclude` "2:14: Definition: <anonymous>::global_anonymous_struct_val [FieldDecl]"
        defsE <- defsAt "structs.cpp" 14 10
        defsE `shouldInclude` "10:16: Definition: main(int, char **)::local_struct_var [VarDecl]"
        defsF <- defsAt "structs.cpp" 14 27
        defsF `shouldInclude` "6:29: Definition: main(int, char **)::local_struct::local_struct_val [FieldDecl]"
        defsG <- defsAt "structs.cpp" 15 10
        defsG `shouldInclude` "6:29: Definition: main(int, char **)::local_struct::local_struct_val [FieldDecl]" -- XXX This doesn't work.
        defsH <- defsAt "structs.cpp" 15 37
        defsH `shouldInclude` "7:16: Definition: main(int, char **)::<anonymous>::local_anonymous_struct_val [FieldDecl]"
    {-
    it "indexes unions" $ do
      withPygd $ do
        index "unions.cpp"
        defs <- defsAt "unions.cpp" 4 10
        defs `shouldInclude` "3:7: Definition: main(int, char **)::identifier [VarDecl]"
    -}
    -- typedefs, C++ classes, templates, enum class, varargs,
    -- namespaces, extern, lamdas, virtual, fields

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
