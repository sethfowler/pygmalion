import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Data.Tuple.Curry
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
  hspec $ around withPygd $ do
  describe "pygindex-clang" $ do
    it "indexes local variables" $ do
        index "local-variables.cpp"
        ("local-variables.cpp", 4, 10) `defShouldBe` "3:7: Definition: main(int, char **)::var [VarDecl]"
    it "indexes global variables" $ do
        index "global-variables.cpp"
        ("global-variables.cpp", 5, 10) `defShouldBe` "1:12: Definition: var [VarDecl]"
    it "indexes preprocessor macros" $ do
        index "preprocessor-macros.cpp"
        ("preprocessor-macros.cpp", 5, 10) `defShouldBe` "1:9: Definition: VAR [MacroDefinition]"
    it "indexes preprocessor macros" $ do
        index "preprocessor-functions.cpp"
        ("preprocessor-functions.cpp", 5, 10) `defShouldBe` "1:9: Definition: VAR [MacroDefinition]"
    it "indexes global functions" $ do
        index "functions.cpp"
        ("functions.cpp", 5, 10) `defShouldBe` "1:5: Definition: var() [FunctionDecl]"
    it "indexes enums" $ do
        index "enums.cpp"
        ("enums.cpp", 8, 10) `defShouldBe` "1:20: Definition: global_enum::global_enum_val [EnumConstantDecl]"
        ("enums.cpp", 9, 10) `defShouldBe` "2:8: Definition: <anonymous>::global_anonymous_enum_val [EnumConstantDecl]"
        ("enums.cpp", 10, 10) `defShouldBe` "6:21: Definition: main(int, char **)::local_enum::local_enum_val [EnumConstantDecl]"
        ("enums.cpp", 11, 10) `defShouldBe` "7:10: Definition: main(int, char **)::<anonymous>::local_anonymous_enum_val [EnumConstantDecl]"
    it "indexes structs" $ do
        index "structs.cpp"
        ("structs.cpp", 12, 10) `defShouldBe` "9:17: Definition: main(int, char **)::global_struct_var [VarDecl]"
        ("structs.cpp", 12, 28) `defShouldBe` "1:28: Definition: global_struct::global_struct_val [FieldDecl]"
        ("structs.cpp", 13, 10) `defShouldBe` "2:45: Definition: global_anonymous_struct_var [VarDecl]"
        ("structs.cpp", 13, 38) `defShouldBe` "2:14: Definition: <anonymous>::global_anonymous_struct_val [FieldDecl]"
        ("structs.cpp", 14, 10) `defShouldBe` "10:16: Definition: main(int, char **)::local_struct_var [VarDecl]"
        ("structs.cpp", 14, 27) `defShouldBe` "6:29: Definition: main(int, char **)::local_struct::local_struct_val [FieldDecl]"
        --("structs.cpp", 15, 10) `defShouldBe` "6:29: Definition: main(int, char **)::local_struct::local_struct_val [FieldDecl]" -- XXX This doesn't work.
        ("structs.cpp", 15, 37) `defShouldBe` "7:16: Definition: main(int, char **)::<anonymous>::local_anonymous_struct_val [FieldDecl]"
    {-
    it "indexes unions" $ do
        index "unions.cpp"
        ("unions.cpp", 4, 10) `defShouldBe` "3:7: Definition: main(int, char **)::identifier [VarDecl]"
    -}
    -- typedefs, C++ classes, templates, enum class, varargs,
    -- namespaces, extern, lamdas, virtual, fields

defShouldBe :: (FilePath, Int, Int) -> String -> Expectation
defShouldBe loc s = do
    ss <- uncurryN defsAt $ loc
    assertBool (errorMsg ss) $ any (s `isInfixOf`) ss
  where
    errorMsg ss = "Definition for " ++ (show loc) ++ " was " ++ show ss ++ "; expected " ++ show s

withPygd :: IO () -> IO ()
withPygd action = bracket startPygd stopPygd (\_ -> action)
  where
    startPygd = do bg $ "../dist/build/pygd/pygd"
                   threadDelay 1000000
    stopPygd _ = do void $ pygmalion ["--stop"]
                    sh $ "rm -f " ++ dbFile
  
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
