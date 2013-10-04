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
      ("virtual-in-structs.cpp", 113, 34) `defShouldBe` "ABC::A_pure_method() [CXXMethod]"
      ("virtual-in-structs.cpp", 114, 34) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 115, 47) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 116, 52) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      ("virtual-in-structs.cpp", 119, 30) `defShouldBe` "XXX"
      ("virtual-in-structs.cpp", 120, 30) `defShouldBe` "XXX"
      ("virtual-in-structs.cpp", 121, 43) `defShouldBe` "XXX"
      ("virtual-in-structs.cpp", 122, 48) `defShouldBe` "XXX"

      -- Structs containing pointers to instances.
      ("virtual-in-structs.cpp", 126, 39) `defShouldBe` "XXX"
      ("virtual-in-structs.cpp", 127, 39) `defShouldBe` "XXX"
      ("virtual-in-structs.cpp", 128, 52) `defShouldBe` "XXX"
      ("virtual-in-structs.cpp", 129, 57) `defShouldBe` "XXX"
      ("virtual-in-structs.cpp", 132, 35) `defShouldBe` "XXX"
      ("virtual-in-structs.cpp", 133, 35) `defShouldBe` "XXX"
      ("virtual-in-structs.cpp", 134, 48) `defShouldBe` "XXX"
      ("virtual-in-structs.cpp", 135, 53) `defShouldBe` "XXX"

    it "finds virtual methods in return values" $ do
      index "virtual-in-return-value.cpp"

      -- Functions returning values or pointers.
      ("virtual-in-return-value.cpp", 138, 28) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 139, 28) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 140, 41) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 141, 46) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 143, 28) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 144, 28) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 145, 41) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 146, 46) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 148, 29) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 149, 29) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 150, 42) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 151, 47) `defShouldBe` "XXX"

      -- Function pointers to functions returning values or pointers.
      ("virtual-in-return-value.cpp", 154, 54) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 155, 35) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 156, 35) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 157, 48) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 158, 53) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 161, 35) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 162, 35) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 163, 48) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 164, 53) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 167, 36) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 168, 36) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 169, 49) `defShouldBe` "XXX"
      ("virtual-in-return-value.cpp", 170, 54) `defShouldBe` "XXX"

    it "finds virtual methods which some classes don't override" $ do
      index "virtual-no-override.cpp"

      -- Classes which don't override a virtual method defined in an ancestor class.
      ("virtual-no-override.cpp", 174, 35) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 175, 35) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 176, 48) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 177, 65) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 180, 31) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 181, 31) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 182, 44) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 183, 61) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 183, 61) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 186, 30) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 187, 30) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 188, 43) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 189, 60) `defShouldBe` "XXX"

      -- Classes which have a direct ancestor which didn't override a method.
      ("virtual-no-override.cpp", 193, 28) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 194, 28) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 195, 41) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 196, 58) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 197, 51) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 200, 24) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 201, 24) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 202, 37) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 203, 54) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 204, 47) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 207, 23) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 208, 23) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 209, 36) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 210, 53) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 211, 46) `defShouldBe` "XXX"

      -- Classes which have an indirect ancestor which didn't override a method.
      ("virtual-no-override.cpp", 215, 33) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 216, 33) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 217, 46) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 218, 63) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 219, 56) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 220, 61) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 223, 29) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 224, 29) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 225, 42) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 226, 59) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 227, 52) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 228, 57) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 231, 28) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 232, 28) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 233, 41) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 234, 58) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 235, 51) `defShouldBe` "XXX"
      ("virtual-no-override.cpp", 236, 56) `defShouldBe` "XXX"

    -- typedefs, templates, bitfields, type refs in cast expressions,
    -- namespaces, extern, lambdas, multiple inheritance, operator overloads, function ptrs
    -- inherited fields and static members
    -- need to add tests for 'find references', 'bases', 'overrides', etc.
    -- remember to ensure that find references works with macro expansions!
