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
runTests = hspec $ around withPygd $

  describe "pygindex-clang" $ do

    it "indexes variables" $ do
      index "variables.cpp"
      ("variables.cpp", 9, 10) `defShouldBe` "1:12: Definition: global_var [VarDecl]"
      ("variables.cpp", 10, 10) `defShouldBe` "2:18: Definition: global_const_var [VarDecl]"
      ("variables.cpp", 11, 10) `defShouldBe` "6:7: Definition: main(int, char **)::local_var [VarDecl]"
      ("variables.cpp", 12, 10) `defShouldBe` "7:13: Definition: main(int, char **)::local_const_var [VarDecl]"

    it "indexes functions" $ do
      index "functions.cpp"
      ("functions.cpp", 6, 10) `defShouldBe` "1:5: Definition: var() [FunctionDecl]"
      ("functions.cpp", 7, 10) `defShouldBe` "2:5: Definition: varargs(int, ...) [FunctionDecl]"

    it "indexes macros" $ do
      index "macros.cpp"
      ("macros.cpp", 8, 10) `defShouldBe` "1:9: Definition: VAR [MacroDefinition]"
      ("macros.cpp", 9, 10) `defShouldBe` "2:9: Definition: VARF [MacroDefinition]"
      -- ("macros.cpp", 9, 15) `defShouldBe` "6:8: Definition: main(int, char **)::local_var [VarDecl]"

    it "indexes enums" $ do
      index "enums.cpp"
      ("enums.cpp", 11, 3) `defShouldBe` "1:6: Definition: global_enum [EnumDecl]"
      ("enums.cpp", 12, 3) `defShouldBe` "3:12: Definition: global_enum_class [EnumDecl]"
      ("enums.cpp", 13, 3) `defShouldBe` "7:8: Definition: main(int, char **)::local_enum [EnumDecl]"
      ("enums.cpp", 14, 3) `defShouldBe` "9:14: Definition: main(int, char **)::local_enum_class [EnumDecl]"
      ("enums.cpp", 15, 13) `defShouldBe` "11:15: Definition: main(int, char **)::global_enum_var [VarDecl]"
      ("enums.cpp", 16, 13) `defShouldBe` "2:36: Definition: global_anonymous_enum_var [VarDecl]"
      ("enums.cpp", 17, 30) `defShouldBe` "12:21: Definition: main(int, char **)::global_enum_class_var [VarDecl]"
      ("enums.cpp", 18, 13) `defShouldBe` "13:14: Definition: main(int, char **)::local_enum_var [VarDecl]"
      ("enums.cpp", 19, 13) `defShouldBe` "8:37: Definition: main(int, char **)::local_anonymous_enum_var [VarDecl]"
      ("enums.cpp", 20, 30) `defShouldBe` "14:20: Definition: main(int, char **)::local_enum_class_var [VarDecl]"
      ("enums.cpp", 22, 10) `defShouldBe` "1:20: Definition: global_enum::global_enum_val [EnumConstantDecl]"
      ("enums.cpp", 23, 10) `defShouldBe` "2:8: Definition: <anonymous>::global_anonymous_enum_val [EnumConstantDecl]"
      ("enums.cpp", 24, 27) `defShouldBe` "3:12: Definition: global_enum_class [EnumDecl]"
      ("enums.cpp", 24, 46) `defShouldBe` "3:38: Definition: global_enum_class::global_enum_class_val [EnumConstantDecl]"
      ("enums.cpp", 25, 10) `defShouldBe` "7:21: Definition: main(int, char **)::local_enum::local_enum_val [EnumConstantDecl]"
      ("enums.cpp", 26, 10) `defShouldBe` "8:10: Definition: main(int, char **)::<anonymous>::local_anonymous_enum_val [EnumConstantDecl]"
      ("enums.cpp", 27, 27) `defShouldBe` "9:14: Definition: main(int, char **)::local_enum_class [EnumDecl]"
      ("enums.cpp", 27, 45) `defShouldBe` "9:39: Definition: main(int, char **)::local_enum_class::local_enum_class_val [EnumConstantDecl]"

    it "indexes structs" $ do
      index "structs.cpp"
      ("structs.cpp", 12, 10) `defShouldBe` "9:17: Definition: main(int, char **)::global_struct_var [VarDecl]"
      ("structs.cpp", 12, 28) `defShouldBe` "1:28: Definition: global_struct::global_struct_val [FieldDecl]"
      ("structs.cpp", 13, 10) `defShouldBe` "2:45: Definition: global_anonymous_struct_var [VarDecl]"
      ("structs.cpp", 13, 38) `defShouldBe` "2:14: Definition: <anonymous>::global_anonymous_struct_val [FieldDecl]"
      ("structs.cpp", 14, 10) `defShouldBe` "10:16: Definition: main(int, char **)::local_struct_var [VarDecl]"
      ("structs.cpp", 14, 27) `defShouldBe` "6:29: Definition: main(int, char **)::local_struct::local_struct_val [FieldDecl]"
      ("structs.cpp", 15, 10) `defShouldBe` "7:46: Definition: main(int, char **)::local_anonymous_struct_var [VarDecl]"
      ("structs.cpp", 15, 37) `defShouldBe` "7:16: Definition: main(int, char **)::<anonymous>::local_anonymous_struct_val [FieldDecl]"

    it "indexes unions" $ do
      index "unions.cpp"
      ("unions.cpp", 27, 3) `defShouldBe` "1:7: Definition: global_union [UnionDecl]"
      ("unions.cpp", 28, 3) `defShouldBe` "15:9: Definition: main(int, char **)::local_union [UnionDecl]"
      ("unions.cpp", 30, 10) `defShouldBe` "16: Definition: main(int, char **)::global_union_var [VarDecl]"
      ("unions.cpp", 30, 27) `defShouldBe` "3:7: Definition: global_union::global_union_val_int [FieldDecl]"
      ("unions.cpp", 31, 27) `defShouldBe` "4:8: Definition: global_union::global_union_val_char [FieldDecl]"
      ("unions.cpp", 32, 10) `defShouldBe` "11:3: Definition: global_anonymous_union_var [VarDecl]"
      ("unions.cpp", 32, 37) `defShouldBe` "9:7: Definition: <anonymous>::global_anonymous_union_val_int [FieldDecl]"
      ("unions.cpp", 33, 37) `defShouldBe` "10:8: Definition: <anonymous>::global_anonymous_union_val_char [FieldDecl]"
      ("unions.cpp", 34, 10) `defShouldBe` "28:15: Definition: main(int, char **)::local_union_var [VarDecl]"
      ("unions.cpp", 34, 26) `defShouldBe` "17:9: Definition: main(int, char **)::local_union::local_union_val_int [FieldDecl]"
      ("unions.cpp", 35, 26) `defShouldBe` "18:10: Definition: main(int, char **)::local_union::local_union_val_char [FieldDecl]"
      ("unions.cpp", 36, 10) `defShouldBe` "25:5: Definition: main(int, char **)::local_anonymous_union_var [VarDecl]"
      ("unions.cpp", 36, 36) `defShouldBe` "23:9: Definition: main(int, char **)::<anonymous>::local_anonymous_union_val_int [FieldDecl]"
      ("unions.cpp", 37, 36) `defShouldBe` "24:10: Definition: main(int, char **)::<anonymous>::local_anonymous_union_val_char [FieldDecl]"

    it "indexes classes" $ do
      index "classes.cpp"
      ("classes.cpp", 51, 3) `defShouldBe` "1:7: Definition: global_class [ClassDecl]"
      --("classes.cpp", 51, 16) `defShouldBe` "XXX constructors don't work"
      ("classes.cpp", 52, 3) `defShouldBe` "1:7: Definition: global_class [ClassDecl]"
      --("classes.cpp", 52, 16) `defShouldBe` "XXX constructors don't work"
      ("classes.cpp", 53, 3) `defShouldBe` "37:9: Definition: main(int, char **)::local_class [ClassDecl]"
      --("classes.cpp", 53, 15) `defShouldBe` "XXX constructors don't work"
      ("classes.cpp", 54, 3) `defShouldBe` "1:7: Definition: global_class [ClassDecl]"
      ("classes.cpp", 54, 17) `defShouldBe` "13:9: Definition: global_class::nested_class [ClassDecl]"
      --("classes.cpp", 54, 30) `defShouldBe` "XXX constructors don't work"
      ("classes.cpp", 55, 17) `defShouldBe` "21:9: Definition: global_class::nested_union [UnionDecl]"
      ("classes.cpp", 56, 17) `defShouldBe` "27:8: Definition: global_class::nested_enum [EnumDecl]"
      ("classes.cpp", 58, 13) `defShouldBe` "1:7: Definition: global_class [ClassDecl]"
      ("classes.cpp", 58, 27) `defShouldBe` "4:14: Definition: global_class::static_method(int) [CXXMethod]"
      ("classes.cpp", 59, 27) `defShouldBe` "5:20: Definition: global_class::static_field [VarDecl]"
      ("classes.cpp", 60, 27) `defShouldBe` "13:9: Definition: global_class::nested_class [ClassDecl]"
      ("classes.cpp", 60, 41) `defShouldBe` "16:22: Definition: global_class::nested_class::nested_static_field [VarDecl]"
      ("classes.cpp", 61, 27) `defShouldBe` "29:5: Definition: global_class::nested_enum::nested_enum_val [EnumConstantDecl]"
      ("classes.cpp", 63, 10) `defShouldBe` "33:14: Definition: global_instance [VarDecl]"
      ("classes.cpp", 63, 26) `defShouldBe` "11:7: Definition: global_class::field [FieldDecl]"
      ("classes.cpp", 64, 26) `defShouldBe` "10:7: Definition: global_class::method(int) [CXXMethod]"
      ("classes.cpp", 65, 10) `defShouldBe` "51:16: Definition: main(int, char **)::local_instance [VarDecl]"
      ("classes.cpp", 65, 25) `defShouldBe` "11:7: Definition: global_class::field [FieldDecl]"
      ("classes.cpp", 66, 25) `defShouldBe` "10:7: Definition: global_class::method(int) [CXXMethod]"
      ("classes.cpp", 67, 10) `defShouldBe` "54:30: Definition: main(int, char **)::nested_instance [VarDecl]"
      ("classes.cpp", 67, 26) `defShouldBe` "18:9: Definition: global_class::nested_class::nested_field [FieldDecl]"
      ("classes.cpp", 68, 26) `defShouldBe` "17:9: Definition: global_class::nested_class::nested_method(int) [CXXMethod]"
      ("classes.cpp", 69, 10) `defShouldBe` "55:30: Definition: main(int, char **)::nested_union_var [VarDecl]"
      ("classes.cpp", 69, 27) `defShouldBe` "23:9: Definition: global_class::nested_union::nested_union_val_int [FieldDecl]"
      ("classes.cpp", 70, 27) `defShouldBe` "24:10: Definition: global_class::nested_union::nested_union_val_char [FieldDecl]"
      ("classes.cpp", 71, 10) `defShouldBe` "53:15: Definition: main(int, char **)::local_class_instance [VarDecl]"
      ("classes.cpp", 71, 31) `defShouldBe` "41:9: Definition: main(int, char **)::local_class::local_field [FieldDecl]"
      ("classes.cpp", 72, 31) `defShouldBe` "40:9: Definition: main(int, char **)::local_class::local_method(int) [CXXMethod]"
      ("classes.cpp", 73, 10) `defShouldBe` "49:5: Definition: main(int, char **)::anonymous_instance [VarDecl]"
      ("classes.cpp", 73, 29) `defShouldBe` "48:9: Definition: main(int, char **)::<anonymous>::anonymous_field [FieldDecl]"
      ("classes.cpp", 74, 29) `defShouldBe` "47:9: Definition: main(int, char **)::<anonymous>::anonymous_method(int) [CXXMethod]"

    it "indexes virtual methods" $ do
      index "virtual.cpp"

      -- Instance values.
      ("virtual.cpp", 60, 3) `defShouldBe` "XXX"
      ("virtual.cpp", 61, 3) `defShouldBe` "XXX"
      ("virtual.cpp", 61, 18) `defShouldBe` "XXX"
      ("virtual.cpp", 62, 18) `defShouldBe` "XXX"
      ("virtual.cpp", 65, 23) `defShouldBe` "XXX"
      ("virtual.cpp", 66, 23) `defShouldBe` "XXX"
      ("virtual.cpp", 67, 23) `defShouldBe` "XXX"
      ("virtual.cpp", 67, 36) `defShouldBe` "XXX"
      ("virtual.cpp", 68, 23) `defShouldBe` "XXX"
      ("virtual.cpp", 68, 41) `defShouldBe` "XXX"

      -- Pointers to instances.
      ("virtual.cpp", 71, 3) `defShouldBe` "XXX"
      ("virtual.cpp", 72, 14) `defShouldBe` "XXX"
      ("virtual.cpp", 73, 16) `defShouldBe` "XXX"
      ("virtual.cpp", 74, 14) `defShouldBe` "XXX"
      ("virtual.cpp", 75, 16) `defShouldBe` "XXX"
      ("virtual.cpp", 78, 19) `defShouldBe` "XXX"
      ("virtual.cpp", 79, 21) `defShouldBe` "XXX"
      ("virtual.cpp", 80, 19) `defShouldBe` "XXX"
      ("virtual.cpp", 81, 21) `defShouldBe` "XXX"
      ("virtual.cpp", 82, 32) `defShouldBe` "XXX"
      ("virtual.cpp", 83, 34) `defShouldBe` "XXX"
      ("virtual.cpp", 84, 37) `defShouldBe` "XXX"
      ("virtual.cpp", 85, 39) `defShouldBe` "XXX"
      ("virtual.cpp", 88, 28) `defShouldBe` "XXX"
      ("virtual.cpp", 89, 30) `defShouldBe` "XXX"
      ("virtual.cpp", 90, 28) `defShouldBe` "XXX"
      ("virtual.cpp", 91, 30) `defShouldBe` "XXX"
      ("virtual.cpp", 92, 41) `defShouldBe` "XXX"
      ("virtual.cpp", 93, 43) `defShouldBe` "XXX"

      -- References to instances.
      ("virtual.cpp", 96, 3) `defShouldBe` "XXX"
      ("virtual.cpp", 97, 22) `defShouldBe` "XXX"
      ("virtual.cpp", 98, 22) `defShouldBe` "XXX"
      ("virtual.cpp", 101, 27) `defShouldBe` "XXX"
      ("virtual.cpp", 102, 27) `defShouldBe` "XXX"
      ("virtual.cpp", 103, 40) `defShouldBe` "XXX"
      ("virtual.cpp", 104, 45) `defShouldBe` "XXX"
      ("virtual.cpp", 107, 27) `defShouldBe` "XXX"
      ("virtual.cpp", 108, 27) `defShouldBe` "XXX"
      ("virtual.cpp", 109, 40) `defShouldBe` "XXX"

      -- Structs containing instance values.
      ("virtual.cpp", 113, 34) `defShouldBe` "XXX"
      ("virtual.cpp", 114, 34) `defShouldBe` "XXX"
      ("virtual.cpp", 115, 47) `defShouldBe` "XXX"
      ("virtual.cpp", 116, 52) `defShouldBe` "XXX"
      ("virtual.cpp", 119, 30) `defShouldBe` "XXX"
      ("virtual.cpp", 120, 30) `defShouldBe` "XXX"
      ("virtual.cpp", 121, 43) `defShouldBe` "XXX"
      ("virtual.cpp", 122, 48) `defShouldBe` "XXX"

      -- Structs containing pointers to instances.
      ("virtual.cpp", 126, 39) `defShouldBe` "XXX"
      ("virtual.cpp", 127, 39) `defShouldBe` "XXX"
      ("virtual.cpp", 128, 52) `defShouldBe` "XXX"
      ("virtual.cpp", 129, 57) `defShouldBe` "XXX"
      ("virtual.cpp", 132, 35) `defShouldBe` "XXX"
      ("virtual.cpp", 133, 35) `defShouldBe` "XXX"
      ("virtual.cpp", 134, 48) `defShouldBe` "XXX"
      ("virtual.cpp", 135, 53) `defShouldBe` "XXX"

      -- Functions returning values or pointers.
      ("virtual.cpp", 138, 28) `defShouldBe` "XXX"
      ("virtual.cpp", 139, 28) `defShouldBe` "XXX"
      ("virtual.cpp", 140, 41) `defShouldBe` "XXX"
      ("virtual.cpp", 141, 46) `defShouldBe` "XXX"
      ("virtual.cpp", 143, 28) `defShouldBe` "XXX"
      ("virtual.cpp", 144, 28) `defShouldBe` "XXX"
      ("virtual.cpp", 145, 41) `defShouldBe` "XXX"
      ("virtual.cpp", 146, 46) `defShouldBe` "XXX"
      ("virtual.cpp", 148, 29) `defShouldBe` "XXX"
      ("virtual.cpp", 149, 29) `defShouldBe` "XXX"
      ("virtual.cpp", 150, 42) `defShouldBe` "XXX"
      ("virtual.cpp", 151, 47) `defShouldBe` "XXX"

      -- Function pointers to functions returning values or pointers.
      ("virtual.cpp", 154, 54) `defShouldBe` "XXX"
      ("virtual.cpp", 155, 35) `defShouldBe` "XXX"
      ("virtual.cpp", 156, 35) `defShouldBe` "XXX"
      ("virtual.cpp", 157, 48) `defShouldBe` "XXX"
      ("virtual.cpp", 158, 53) `defShouldBe` "XXX"
      ("virtual.cpp", 161, 35) `defShouldBe` "XXX"
      ("virtual.cpp", 162, 35) `defShouldBe` "XXX"
      ("virtual.cpp", 163, 48) `defShouldBe` "XXX"
      ("virtual.cpp", 164, 53) `defShouldBe` "XXX"
      ("virtual.cpp", 167, 36) `defShouldBe` "XXX"
      ("virtual.cpp", 168, 36) `defShouldBe` "XXX"
      ("virtual.cpp", 169, 49) `defShouldBe` "XXX"
      ("virtual.cpp", 170, 54) `defShouldBe` "XXX"

      -- Classes which don't override a virtual method defined in an ancestor class.
      ("virtual.cpp", 174, 35) `defShouldBe` "XXX"
      ("virtual.cpp", 175, 35) `defShouldBe` "XXX"
      ("virtual.cpp", 176, 48) `defShouldBe` "XXX"
      ("virtual.cpp", 177, 65) `defShouldBe` "XXX"
      ("virtual.cpp", 180, 31) `defShouldBe` "XXX"
      ("virtual.cpp", 181, 31) `defShouldBe` "XXX"
      ("virtual.cpp", 182, 44) `defShouldBe` "XXX"
      ("virtual.cpp", 183, 61) `defShouldBe` "XXX"
      ("virtual.cpp", 183, 61) `defShouldBe` "XXX"
      ("virtual.cpp", 186, 30) `defShouldBe` "XXX"
      ("virtual.cpp", 187, 30) `defShouldBe` "XXX"
      ("virtual.cpp", 188, 43) `defShouldBe` "XXX"
      ("virtual.cpp", 189, 60) `defShouldBe` "XXX"

      -- Classes which have a direct ancestor which didn't override a method.
      ("virtual.cpp", 193, 28) `defShouldBe` "XXX"
      ("virtual.cpp", 194, 28) `defShouldBe` "XXX"
      ("virtual.cpp", 195, 41) `defShouldBe` "XXX"
      ("virtual.cpp", 196, 58) `defShouldBe` "XXX"
      ("virtual.cpp", 197, 51) `defShouldBe` "XXX"
      ("virtual.cpp", 200, 24) `defShouldBe` "XXX"
      ("virtual.cpp", 201, 24) `defShouldBe` "XXX"
      ("virtual.cpp", 202, 37) `defShouldBe` "XXX"
      ("virtual.cpp", 203, 54) `defShouldBe` "XXX"
      ("virtual.cpp", 204, 47) `defShouldBe` "XXX"
      ("virtual.cpp", 207, 23) `defShouldBe` "XXX"
      ("virtual.cpp", 208, 23) `defShouldBe` "XXX"
      ("virtual.cpp", 209, 36) `defShouldBe` "XXX"
      ("virtual.cpp", 210, 53) `defShouldBe` "XXX"
      ("virtual.cpp", 211, 46) `defShouldBe` "XXX"

      -- Classes which have an indirect ancestor which didn't override a method.
      ("virtual.cpp", 215, 33) `defShouldBe` "XXX"
      ("virtual.cpp", 216, 33) `defShouldBe` "XXX"
      ("virtual.cpp", 217, 46) `defShouldBe` "XXX"
      ("virtual.cpp", 218, 63) `defShouldBe` "XXX"
      ("virtual.cpp", 219, 56) `defShouldBe` "XXX"
      ("virtual.cpp", 220, 61) `defShouldBe` "XXX"
      ("virtual.cpp", 223, 29) `defShouldBe` "XXX"
      ("virtual.cpp", 224, 29) `defShouldBe` "XXX"
      ("virtual.cpp", 225, 42) `defShouldBe` "XXX"
      ("virtual.cpp", 226, 59) `defShouldBe` "XXX"
      ("virtual.cpp", 227, 52) `defShouldBe` "XXX"
      ("virtual.cpp", 228, 57) `defShouldBe` "XXX"
      ("virtual.cpp", 231, 28) `defShouldBe` "XXX"
      ("virtual.cpp", 232, 28) `defShouldBe` "XXX"
      ("virtual.cpp", 233, 41) `defShouldBe` "XXX"
      ("virtual.cpp", 234, 58) `defShouldBe` "XXX"
      ("virtual.cpp", 235, 51) `defShouldBe` "XXX"
      ("virtual.cpp", 236, 56) `defShouldBe` "XXX"

    -- typedefs, templates, bitfields, type refs in cast expressions,
    -- namespaces, extern, lambdas, multiple inheritance, operator overloads, function ptrs
    -- inherited fields and static members
    -- need to add tests for 'find references', 'bases', 'overrides', etc.
    -- remember to ensure that find references works with macro expansions!
      

defShouldBe :: (FilePath, Int, Int) -> String -> Expectation
defShouldBe loc s = do
    ss <- uncurryN defsAt loc
    assertBool (errorMsg ss) $ any (s `isInfixOf`) ss
  where
    errorMsg ss = "Definition for " ++ show loc ++ " was " ++ show ss ++ "; expected " ++ show s

withPygd :: IO () -> IO ()
withPygd action = bracket startPygd stopPygd (const action)
  where
    startPygd = do bg "../dist/build/pygd/pygd"
                   threadDelay 1000000
    stopPygd _ = do void $ pygmalion ["--stop"]
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
  void $ pygmalion ["--index", "clang++", "--std=c++11", file]
  threadDelay 1000000

defsAt :: FilePath -> Int -> Int -> IO [String]
defsAt file line col = pygmalion ["--definition", file, show line, show col]

sh :: String -> IO ()
sh cmd = void $ waitForProcess =<< runCommand cmd

bg :: String -> IO ()
bg cmd = void $ runCommand cmd
