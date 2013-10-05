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

import Pygmalion.Test (defShouldBe, defsShouldBe, index, withPygd)

main :: IO ()
main = setCurrentDirectory "tests" >> runTests

runTests :: IO ()
runTests = hspec $ around withPygd $

  describe "go-to-definition" $ do

    it "finds variables" $ do
      index "variables.cpp"
      ("variables.cpp", 9, 10) `defShouldBe` "global_var [VarDecl]"
      ("variables.cpp", 10, 10) `defShouldBe` "global_const_var [VarDecl]"
      ("variables.cpp", 11, 10) `defShouldBe` "main(int, char **)::local_var [VarDecl]"
      ("variables.cpp", 12, 10) `defShouldBe` "main(int, char **)::local_const_var [VarDecl]"

    it "finds functions" $ do
      index "functions.cpp"
      ("functions.cpp", 6, 10) `defShouldBe` "var() [FunctionDecl]"
      ("functions.cpp", 7, 10) `defShouldBe` "varargs(int, ...) [FunctionDecl]"

    it "finds macros" $ do
      index "macros.cpp"
      ("macros.cpp", 8, 10) `defShouldBe` "VAR [MacroDefinition]"
      ("macros.cpp", 9, 10) `defShouldBe` "VARF [MacroDefinition]"
      -- ("macros.cpp", 9, 15) `defShouldBe` "main(int, char **)::local_var [VarDecl]"

    it "finds enums" $ do
      index "enums.cpp"
      ("enums.cpp", 11, 3) `defShouldBe` "global_enum [EnumDecl]"
      ("enums.cpp", 12, 3) `defShouldBe` "global_enum_class [EnumDecl]"
      ("enums.cpp", 13, 3) `defShouldBe` "main(int, char **)::local_enum [EnumDecl]"
      ("enums.cpp", 14, 3) `defShouldBe` "main(int, char **)::local_enum_class [EnumDecl]"
      ("enums.cpp", 15, 13) `defShouldBe` "main(int, char **)::global_enum_var [VarDecl]"
      ("enums.cpp", 16, 13) `defShouldBe` "global_anonymous_enum_var [VarDecl]"
      ("enums.cpp", 17, 30) `defShouldBe` "main(int, char **)::global_enum_class_var [VarDecl]"
      ("enums.cpp", 18, 13) `defShouldBe` "main(int, char **)::local_enum_var [VarDecl]"
      ("enums.cpp", 19, 13) `defShouldBe` "main(int, char **)::local_anonymous_enum_var [VarDecl]"
      ("enums.cpp", 20, 30) `defShouldBe` "main(int, char **)::local_enum_class_var [VarDecl]"
      ("enums.cpp", 22, 10) `defShouldBe` "global_enum::global_enum_val [EnumConstantDecl]"
      ("enums.cpp", 23, 10) `defShouldBe` "<anonymous>::global_anonymous_enum_val [EnumConstantDecl]"
      ("enums.cpp", 24, 27) `defShouldBe` "global_enum_class [EnumDecl]"
      ("enums.cpp", 24, 46) `defShouldBe` "global_enum_class::global_enum_class_val [EnumConstantDecl]"
      ("enums.cpp", 25, 10) `defShouldBe` "main(int, char **)::local_enum::local_enum_val [EnumConstantDecl]"
      ("enums.cpp", 26, 10) `defShouldBe` "main(int, char **)::<anonymous>::local_anonymous_enum_val [EnumConstantDecl]"
      ("enums.cpp", 27, 27) `defShouldBe` "main(int, char **)::local_enum_class [EnumDecl]"
      ("enums.cpp", 27, 45) `defShouldBe` "main(int, char **)::local_enum_class::local_enum_class_val [EnumConstantDecl]"

    it "finds structs" $ do
      index "structs.cpp"
      ("structs.cpp", 12, 10) `defShouldBe` "main(int, char **)::global_struct_var [VarDecl]"
      ("structs.cpp", 12, 28) `defShouldBe` "global_struct::global_struct_val [FieldDecl]"
      ("structs.cpp", 13, 10) `defShouldBe` "global_anonymous_struct_var [VarDecl]"
      ("structs.cpp", 13, 38) `defShouldBe` "<anonymous>::global_anonymous_struct_val [FieldDecl]"
      ("structs.cpp", 14, 10) `defShouldBe` "main(int, char **)::local_struct_var [VarDecl]"
      ("structs.cpp", 14, 27) `defShouldBe` "main(int, char **)::local_struct::local_struct_val [FieldDecl]"
      ("structs.cpp", 15, 10) `defShouldBe` "main(int, char **)::local_anonymous_struct_var [VarDecl]"
      ("structs.cpp", 15, 37) `defShouldBe` "main(int, char **)::<anonymous>::local_anonymous_struct_val [FieldDecl]"

    it "finds unions" $ do
      index "unions.cpp"
      ("unions.cpp", 27, 3) `defShouldBe` "global_union [UnionDecl]"
      ("unions.cpp", 28, 3) `defShouldBe` "main(int, char **)::local_union [UnionDecl]"
      ("unions.cpp", 30, 10) `defShouldBe` "main(int, char **)::global_union_var [VarDecl]"
      ("unions.cpp", 30, 27) `defShouldBe` "global_union::global_union_val_int [FieldDecl]"
      ("unions.cpp", 31, 27) `defShouldBe` "global_union::global_union_val_char [FieldDecl]"
      ("unions.cpp", 32, 10) `defShouldBe` "global_anonymous_union_var [VarDecl]"
      ("unions.cpp", 32, 37) `defShouldBe` "<anonymous>::global_anonymous_union_val_int [FieldDecl]"
      ("unions.cpp", 33, 37) `defShouldBe` "<anonymous>::global_anonymous_union_val_char [FieldDecl]"
      ("unions.cpp", 34, 10) `defShouldBe` "main(int, char **)::local_union_var [VarDecl]"
      ("unions.cpp", 34, 26) `defShouldBe` "main(int, char **)::local_union::local_union_val_int [FieldDecl]"
      ("unions.cpp", 35, 26) `defShouldBe` "main(int, char **)::local_union::local_union_val_char [FieldDecl]"
      ("unions.cpp", 36, 10) `defShouldBe` "main(int, char **)::local_anonymous_union_var [VarDecl]"
      ("unions.cpp", 36, 36) `defShouldBe` "main(int, char **)::<anonymous>::local_anonymous_union_val_int [FieldDecl]"
      ("unions.cpp", 37, 36) `defShouldBe` "main(int, char **)::<anonymous>::local_anonymous_union_val_char [FieldDecl]"

    it "finds classes" $ do
      index "classes.cpp"
      ("classes.cpp", 51, 3) `defShouldBe` "global_class [ClassDecl]"
      --("classes.cpp", 51, 16) `defShouldBe` "XXX constructors don't work"
      ("classes.cpp", 52, 3) `defShouldBe` "global_class [ClassDecl]"
      --("classes.cpp", 52, 16) `defShouldBe` "XXX constructors don't work"
      ("classes.cpp", 53, 3) `defShouldBe` "main(int, char **)::local_class [ClassDecl]"
      --("classes.cpp", 53, 15) `defShouldBe` "XXX constructors don't work"
      ("classes.cpp", 54, 3) `defShouldBe` "global_class [ClassDecl]"
      ("classes.cpp", 54, 17) `defShouldBe` "global_class::nested_class [ClassDecl]"
      --("classes.cpp", 54, 30) `defShouldBe` "XXX constructors don't work"
      ("classes.cpp", 55, 17) `defShouldBe` "global_class::nested_union [UnionDecl]"
      ("classes.cpp", 56, 17) `defShouldBe` "global_class::nested_enum [EnumDecl]"
      ("classes.cpp", 58, 13) `defShouldBe` "global_class [ClassDecl]"
      ("classes.cpp", 58, 27) `defShouldBe` "global_class::static_method(int) [CXXMethod]"
      ("classes.cpp", 59, 27) `defShouldBe` "global_class::static_field [VarDecl]"
      ("classes.cpp", 60, 27) `defShouldBe` "global_class::nested_class [ClassDecl]"
      ("classes.cpp", 60, 41) `defShouldBe` "global_class::nested_class::nested_static_field [VarDecl]"
      ("classes.cpp", 61, 27) `defShouldBe` "global_class::nested_enum::nested_enum_val [EnumConstantDecl]"
      ("classes.cpp", 63, 10) `defShouldBe` "global_instance [VarDecl]"
      ("classes.cpp", 63, 26) `defShouldBe` "global_class::field [FieldDecl]"
      ("classes.cpp", 64, 26) `defShouldBe` "global_class::method(int) [CXXMethod]"
      ("classes.cpp", 65, 10) `defShouldBe` "main(int, char **)::local_instance [VarDecl]"
      ("classes.cpp", 65, 25) `defShouldBe` "global_class::field [FieldDecl]"
      ("classes.cpp", 66, 25) `defShouldBe` "global_class::method(int) [CXXMethod]"
      ("classes.cpp", 67, 10) `defShouldBe` "main(int, char **)::nested_instance [VarDecl]"
      ("classes.cpp", 67, 26) `defShouldBe` "global_class::nested_class::nested_field [FieldDecl]"
      ("classes.cpp", 68, 26) `defShouldBe` "global_class::nested_class::nested_method(int) [CXXMethod]"
      ("classes.cpp", 69, 10) `defShouldBe` "main(int, char **)::nested_union_var [VarDecl]"
      ("classes.cpp", 69, 27) `defShouldBe` "global_class::nested_union::nested_union_val_int [FieldDecl]"
      ("classes.cpp", 70, 27) `defShouldBe` "global_class::nested_union::nested_union_val_char [FieldDecl]"
      ("classes.cpp", 71, 10) `defShouldBe` "main(int, char **)::local_class_instance [VarDecl]"
      ("classes.cpp", 71, 31) `defShouldBe` "main(int, char **)::local_class::local_field [FieldDecl]"
      ("classes.cpp", 72, 31) `defShouldBe` "main(int, char **)::local_class::local_method(int) [CXXMethod]"
      ("classes.cpp", 73, 10) `defShouldBe` "main(int, char **)::anonymous_instance [VarDecl]"
      ("classes.cpp", 73, 29) `defShouldBe` "main(int, char **)::<anonymous>::anonymous_field [FieldDecl]"
      ("classes.cpp", 74, 29) `defShouldBe` "main(int, char **)::<anonymous>::anonymous_method(int) [CXXMethod]"

    it "finds virtual methods" $ do
      index "virtual.cpp"

      -- Instance values.
      ("virtual.cpp", 6, 3) `defShouldBe` "AB [ClassDecl]"
      ("virtual.cpp", 7, 3) `defShouldBe` "main(int, char **)::AB_instance [VarDecl]"
      ("virtual.cpp", 7, 15) `defShouldBe` "AB::A_pure_method() [CXXMethod]"
      ("virtual.cpp", 8, 15) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 11, 3) `defShouldBe` "main(int, char **)::ABC_instance [VarDecl]"
      ("virtual.cpp", 11, 16) `defShouldBe` "ABC::A_pure_method() [CXXMethod]"
      ("virtual.cpp", 12, 16) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 13, 16) `defShouldBe` "AB [ClassDecl]"
      ("virtual.cpp", 13, 20) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 14, 16) `defShouldBe` "ABC [ClassDecl]"
      ("virtual.cpp", 14, 21) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"

      -- Pointers to instances.
      ("virtual.cpp", 17, 3) `defShouldBe` "A [ClassDecl]"
      ("virtual.cpp", 18, 10) `defsShouldBe`
        ["A::A_pure_method() [CXXMethod]",
         "AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual.cpp", 19, 12) `defsShouldBe`
        ["A::A_pure_method() [CXXMethod]",
         "AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual.cpp", 20, 10) `defShouldBe` "A [ClassDecl]"
      ("virtual.cpp", 20, 13) `defShouldBe` "A::A_pure_method() [CXXMethod]"
      ("virtual.cpp", 21, 15) `defShouldBe` "A::A_pure_method() [CXXMethod]"
      ("virtual.cpp", 23, 3) `defShouldBe` "AB [ClassDecl]"
      ("virtual.cpp", 24, 11) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual.cpp", 25, 13) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual.cpp", 26, 11) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual.cpp", 27, 13) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual.cpp", 28, 15) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 28, 17) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 31, 3) `defShouldBe` "ABC [ClassDecl]"
      ("virtual.cpp", 32, 12) `defShouldBe` "ABC::A_pure_method() [CXXMethod]"
      ("virtual.cpp", 33, 14) `defShouldBe` "ABC::A_pure_method() [CXXMethod]"
      ("virtual.cpp", 34, 12) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 35, 14) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 36, 12) `defShouldBe` "AB [ClassDecl]"
      ("virtual.cpp", 36, 16) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 37, 14) `defShouldBe` "AB [ClassDecl]"
      ("virtual.cpp", 37, 18) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 38, 17) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 39, 19) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 42, 18) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual.cpp", 43, 20) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual.cpp", 44, 18) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual.cpp", 45, 20) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual.cpp", 46, 22) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 47, 24) `defShouldBe` "AB::AB_method(int) [CXXMethod]"

      -- References to instances.
      ("virtual.cpp", 50, 3) `defShouldBe` "A [ClassDecl]"
      ("virtual.cpp", 51, 18) `defsShouldBe`
        ["A::A_pure_method() [CXXMethod]",
         "AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual.cpp", 52, 18) `defShouldBe` "A [ClassDecl]"
      ("virtual.cpp", 52, 21) `defShouldBe` "A::A_pure_method() [CXXMethod]"
      ("virtual.cpp", 54, 3) `defShouldBe` "AB [ClassDecl]"
      ("virtual.cpp", 55, 19) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual.cpp", 56, 19) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual.cpp", 57, 23) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 59, 3) `defShouldBe` "ABC [ClassDecl]"
      ("virtual.cpp", 60, 20) `defShouldBe` "ABC::A_pure_method() [CXXMethod]"
      ("virtual.cpp", 61, 20) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 62, 20) `defShouldBe` "AB [ClassDecl]"
      ("virtual.cpp", 62, 24) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 63, 20) `defShouldBe` "ABC [ClassDecl]"
      ("virtual.cpp", 63, 25) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      ("virtual.cpp", 66, 17) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual.cpp", 67, 17) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual.cpp", 68, 21) `defShouldBe` "AB::AB_method(int) [CXXMethod]"

    it "finds virtual methods in struct members" $ do
      index "virtual-in-structs.cpp"

      -- Structs containing instance values.
      ("virtual-in-structs.cpp", 14, 28) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      ("virtual-in-structs.cpp", 15, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 16, 32) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 17, 34) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 20, 24) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      ("virtual-in-structs.cpp", 21, 24) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 22, 28) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 23, 30) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 26, 23) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      ("virtual-in-structs.cpp", 27, 23) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 28, 27) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 29, 29) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Structs containing pointers to instances.
      ("virtual-in-structs.cpp", 33, 33) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-in-structs.cpp", 34, 33) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-in-structs.cpp", 35, 37) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 36, 39) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 39, 29) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-in-structs.cpp", 40, 29) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-in-structs.cpp", 41, 33) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 42, 35) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 45, 28) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-in-structs.cpp", 46, 28) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-in-structs.cpp", 47, 32) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 48, 34) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Structs containing references to instances.
      ("virtual-in-structs.cpp", 52, 32) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-in-structs.cpp", 53, 32) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-in-structs.cpp", 54, 36) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 55, 38) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 58, 28) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-in-structs.cpp", 59, 28) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-in-structs.cpp", 60, 32) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 61, 34) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 64, 27) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-in-structs.cpp", 65, 27) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-in-structs.cpp", 66, 31) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 67, 33) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

    it "finds virtual methods in return values" $ do
      index "virtual-in-return-value.cpp"

      -- Functions returning values or pointers.
      ("virtual-in-return-value.cpp", 33, 22) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      ("virtual-in-return-value.cpp", 34, 22) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 35, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 36, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --("virtual-in-return-value.cpp", 38, 23) `defShouldBe` "XXX" -- Doesn't return all possibilities.
      --("virtual-in-return-value.cpp", 39, 23) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 40, 27) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 41, 29) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --("virtual-in-return-value.cpp", 43, 22) `defShouldBe` "XXX"
      --("virtual-in-return-value.cpp", 44, 22) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 45, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 46, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Function pointers to functions returning values or pointers.
      --("virtual-in-return-value.cpp", 50, 29) `defShouldBe` "XXX" -- Returning by value but we show multiple defs!
      --("virtual-in-return-value.cpp", 51, 29) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 52, 33) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 53, 35) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 56, 30) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-in-return-value.cpp", 57, 30) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-in-return-value.cpp", 58, 34) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 59, 36) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 62, 29) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-in-return-value.cpp", 63, 29) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-in-return-value.cpp", 64, 33) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 65, 35) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Function references to functions returning values or pointers.
      ("virtual-in-return-value.cpp", 69, 22) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      ("virtual-in-return-value.cpp", 70, 22) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 71, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 72, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --("virtual-in-return-value.cpp", 75, 23) `defShouldBe` "XXX" -- Should see multiple defs.
      --("virtual-in-return-value.cpp", 76, 23) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 77, 27) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 78, 29) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --("virtual-in-return-value.cpp", 81, 22) `defShouldBe` "XXX" -- --Should see multiple defs.
      --("virtual-in-return-value.cpp", 82, 22) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 83, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 84, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Methods on instances returning values or pointers.
      ("virtual-in-return-value.cpp", 88, 48) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      ("virtual-in-return-value.cpp", 89, 48) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 90, 52) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 91, 54) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --("virtual-in-return-value.cpp", 93, 49) `defShouldBe` "XXX"
      --("virtual-in-return-value.cpp", 94, 49) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 95, 53) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 96, 55) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --("virtual-in-return-value.cpp", 98, 48) `defShouldBe` "XXX"
      --("virtual-in-return-value.cpp", 99, 48) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 100, 52) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 101, 54) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Methods on pointers to instances returning values or pointers.
      ("virtual-in-return-value.cpp", 105, 44) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      ("virtual-in-return-value.cpp", 106, 44) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 107, 48) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 108, 50) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --("virtual-in-return-value.cpp", 110, 45) `defShouldBe` "XXX"
      --("virtual-in-return-value.cpp", 111, 45) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 112, 49) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 113, 51) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --("virtual-in-return-value.cpp", 115, 44) `defShouldBe` "XXX"
      --("virtual-in-return-value.cpp", 116, 44) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 117, 48) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 118, 50) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Methods on references to instances returning values or pointers.
      ("virtual-in-return-value.cpp", 122, 43) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      ("virtual-in-return-value.cpp", 123, 43) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 124, 47) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 125, 49) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --("virtual-in-return-value.cpp", 127, 44) `defShouldBe` "XXX"
      --("virtual-in-return-value.cpp", 128, 44) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 129, 48) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 130, 50) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --("virtual-in-return-value.cpp", 132, 43) `defShouldBe` "XXX"
      --("virtual-in-return-value.cpp", 133, 43) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 134, 47) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 135, 49) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Invocations of method pointers returning values or pointers.
      ("virtual-in-return-value.cpp", 138, 71) `defShouldBe` "ABDE_container::method_ABDE_by_val() [CXXMethod]"
      --("virtual-in-return-value.cpp", 139, 29) `defShouldBe` "XXX" -- wrong def totally
      --("virtual-in-return-value.cpp", 139, 55) `defShouldBe` --"ABDE::A_pure_method() [CXXMethod]" -- too many defs
      --("virtual-in-return-value.cpp", 140, 55) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 141, 59) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 142, 61) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 145, 56) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-in-return-value.cpp", 146, 56) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-in-return-value.cpp", 147, 60) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 148, 62) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 151, 55) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-in-return-value.cpp", 152, 55) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-in-return-value.cpp", 153, 59) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-return-value.cpp", 154, 61) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

    it "finds virtual methods which some classes don't override" $ do
      index "virtual-no-override.cpp"

      -- Classes which don't override a virtual method defined in an ancestor class.
      ("virtual-no-override.cpp", 7, 16) `defShouldBe` "AB::A_pure_method() [CXXMethod]"
      ("virtual-no-override.cpp", 8, 16) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 9, 20) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 10, 21) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      --("virtual-no-override.cpp", 13, 12) `defShouldBe` "XXX" -- Wrongly includes ABC.
      --("virtual-no-override.cpp", 14, 12) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 15, 16) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 16, 17) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      --("virtual-no-override.cpp", 19, 11) `defShouldBe` "XXX"
      --("virtual-no-override.cpp", 20, 11) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 21, 15) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 22, 16) `defShouldBe` "AB::AB_method(int) [CXXMethod]"

      -- Classes which have an ancestor which didn't override a method.
      ("virtual-no-override.cpp", 26, 17) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      ("virtual-no-override.cpp", 27, 17) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 28, 21) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 29, 22) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 30, 23) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 33, 13) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-no-override.cpp", 34, 13) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-no-override.cpp", 35, 17) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 36, 18) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 37, 19) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 40, 12) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      ("virtual-no-override.cpp", 41, 12) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      ("virtual-no-override.cpp", 42, 16) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 43, 17) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-no-override.cpp", 44, 18) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

    -- easy: typedefs, type refs in cast expressions, namespaces,
      -- extern, function pointers, pointer to method, bitfields
    -- medium: multiple inheritance, operator overloads
      -- inherited fields and static members
    -- worst case scenario: templates

    -- need to add tests for 'find references', 'bases', 'overrides', etc.
    -- remember to ensure that find references works with macro expansions!
