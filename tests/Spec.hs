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

    it "finds variables" $ let f = "variables.cpp" in do
      index f
      (f, 12, 27) `defShouldBe` "global_extern_var [VarDecl]"
      (f, 13, 39) `defShouldBe` "global_extern_const_var [VarDecl]"
      (f, 16, 27) `defShouldBe` "global_extern_ptr [VarDecl]"
      (f, 17, 39) `defShouldBe` "global_extern_const_ptr [VarDecl]"
      (f, 21, 27) `defShouldBe` "global_var [VarDecl]"
      (f, 22, 39) `defShouldBe` "global_const_var [VarDecl]"
      (f, 25, 27) `defShouldBe` "global_ptr [VarDecl]"
      (f, 26, 39) `defShouldBe` "global_const_ptr [VarDecl]"
      (f, 32, 21) `defShouldBe` "main(int, char **)::local_var [VarDecl]"
      (f, 33, 33) `defShouldBe` "main(int, char **)::local_const_var [VarDecl]"
      (f, 36, 21) `defShouldBe` "main(int, char **)::local_ptr [VarDecl]"
      (f, 37, 33) `defShouldBe` "main(int, char **)::local_const_ptr [VarDecl]"
      (f, 39, 10) `defShouldBe` "global_extern_var [VarDecl]"
      (f, 40, 10) `defShouldBe` "global_extern_const_var [VarDecl]"
      (f, 41, 11) `defShouldBe` "global_extern_ptr [VarDecl]"
      (f, 42, 11) `defShouldBe` "global_extern_const_ptr [VarDecl]"
      (f, 43, 11) `defShouldBe` "global_extern_ptr_const [VarDecl]"
      (f, 44, 11) `defShouldBe` "global_extern_const_ptr_const [VarDecl]"
      (f, 45, 10) `defShouldBe` "global_extern_ref [VarDecl]"
      (f, 46, 10) `defShouldBe` "global_extern_const_ref [VarDecl]"
      (f, 47, 10) `defShouldBe` "global_var [VarDecl]"
      (f, 48, 10) `defShouldBe` "global_const_var [VarDecl]"
      (f, 49, 11) `defShouldBe` "global_ptr [VarDecl]"
      (f, 50, 11) `defShouldBe` "global_const_ptr [VarDecl]"
      (f, 51, 11) `defShouldBe` "global_ptr_const [VarDecl]"
      (f, 52, 11) `defShouldBe` "global_const_ptr_const [VarDecl]"
      (f, 53, 10) `defShouldBe` "global_ref [VarDecl]"
      (f, 54, 10) `defShouldBe` "global_const_ref [VarDecl]"
      (f, 55, 10) `defShouldBe` "main(int, char **)::local_var [VarDecl]"
      (f, 56, 10) `defShouldBe` "main(int, char **)::local_const_var [VarDecl]"
      (f, 57, 11) `defShouldBe` "main(int, char **)::local_ptr [VarDecl]"
      (f, 58, 11) `defShouldBe` "main(int, char **)::local_const_ptr [VarDecl]"
      (f, 59, 11) `defShouldBe` "main(int, char **)::local_ptr_const [VarDecl]"
      (f, 60, 11) `defShouldBe` "main(int, char **)::local_const_ptr_const [VarDecl]"
      (f, 61, 10) `defShouldBe` "main(int, char **)::local_ref [VarDecl]"
      (f, 62, 10) `defShouldBe` "main(int, char **)::local_const_ref [VarDecl]"

    it "finds functions" $ let f = "functions.cpp" in do
      index f
      (f, 7, 40) `defShouldBe` "func_varargs(int, ...) [FunctionDecl]"
      (f, 9, 40) `defShouldBe` "func_varargs(int, ...) [FunctionDecl]"
      (f, 11, 10) `defShouldBe` "func() [FunctionDecl]"
      (f, 12, 10) `defShouldBe` "func_varargs(int, ...) [FunctionDecl]"
      (f, 13, 10) `defShouldBe` "main(int, char **)::func_ptr [VarDecl]"
      (f, 14, 12) `defShouldBe` "main(int, char **)::func_ptr_varargs [VarDecl]"
      (f, 15, 10) `defShouldBe` "main(int, char **)::func_ref [VarDecl]"
      (f, 16, 10) `defShouldBe` "main(int, char **)::func_ref_varargs [VarDecl]"

    it "finds macros" $ let f = "macros.cpp" in do
      index f
      (f, 8, 10) `defShouldBe` "VAR [MacroDefinition]"
      (f, 9, 10) `defShouldBe` "VARF [MacroDefinition]"
      -- (f, 9, 15) `defShouldBe` "main(int, char **)::local_var [VarDecl]"

    it "finds typedefs" $ let f = "typedefs.cpp" in do
      index f
      (f, 6, 8) `defShouldBe` "clazz::clazz_typedef [TypedefDecl]"
      (f, 6, 30) `defShouldBe` "ns::ns_typedef [TypedefDecl]"
      (f, 6, 44) `defShouldBe` "global_typedef [TypedefDecl]"
      (f, 12, 26) `defShouldBe` "clazz::clazz_typedef [TypedefDecl]"
      --(f, 12, 42) `defShouldBe` "templat<clazz::clazz_typedef>::templat_typedef [TypedefDecl]"
      (f, 14, 3) `defShouldBe` "func_typedef [TypedefDecl]"
      (f, 15, 10) `defShouldBe` "main(int, char **)::local_typedef [TypedefDecl]"

    it "finds types in cast expressions" $ let f = "casts.cpp" in do
      index f
      (f, 10, 35) `defShouldBe` "int_typedef_type [TypedefDecl]"
      (f, 10, 53) `defShouldBe` "main(int, char **)::int_value [VarDecl]"
      (f, 11, 40) `defShouldBe` "int_typedef_type [TypedefDecl]"
      (f, 11, 58) `defShouldBe` "main(int, char **)::int_value [VarDecl]"
      (f, 12, 24) `defShouldBe` "int_typedef_type [TypedefDecl]"
      (f, 12, 41) `defShouldBe` "main(int, char **)::int_value [VarDecl]"
      (f, 13, 23) `defShouldBe` "int_typedef_type [TypedefDecl]"
      (f, 13, 40) `defShouldBe` "main(int, char **)::int_value [VarDecl]"
      (f, 18, 32) `defShouldBe` "class_type [StructDecl]"
      (f, 18, 44) `defShouldBe` "main(int, char **)::subclass_instance [VarDecl]"
      (f, 19, 21) `defShouldBe` "class_type [StructDecl]"
      (f, 19, 32) `defShouldBe` "main(int, char **)::subclass_instance [VarDecl]"
      (f, 20, 20) `defShouldBe` "class_type [StructDecl]"
      (f, 20, 31) `defShouldBe` "main(int, char **)::subclass_instance [VarDecl]"
      (f, 26, 27) `defShouldBe` "class_type [StructDecl]"
      (f, 26, 40) `defShouldBe` "main(int, char **)::subclass_ptr [VarDecl]"
      (f, 27, 28) `defShouldBe` "class_type [StructDecl]"
      (f, 27, 41) `defShouldBe` "main(int, char **)::subclass_ptr [VarDecl]"
      (f, 28, 32) `defShouldBe` "class_type [StructDecl]"
      (f, 28, 45) `defShouldBe` "main(int, char **)::subclass_ptr [VarDecl]"
      (f, 29, 26) `defShouldBe` "class_type [StructDecl]"
      (f, 29, 39) `defShouldBe` "main(int, char **)::const_class_ptr [VarDecl]"
      (f, 30, 16) `defShouldBe` "class_type [StructDecl]"
      (f, 30, 28) `defShouldBe` "main(int, char **)::subclass_ptr [VarDecl]"
      (f, 31, 16) `defShouldBe` "class_type [StructDecl]"
      (f, 31, 29) `defShouldBe` "main(int, char **)::subclass_ptr [VarDecl]"

    it "finds namespaces" $ let f = "namespaces.cpp" in do
      index f
      (f, 9, 9) `defShouldBe` "ns_b::ns_c [Namespace]"
      --(f, 9, 15) `defShouldBe` "XXX" -- wrong def
      (f, 12, 7) `defShouldBe` "ns_b [Namespace]"
      --(f, 12, 13) `defShouldBe` "XXX" -- no def
      (f, 20, 18) `defShouldBe` "ns_b [Namespace]"
      (f, 24, 17) `defShouldBe` "ns_f [Namespace]"
      (f, 29, 9) `defShouldBe` "ns_b [Namespace]"
      (f, 29, 15) `defShouldBe` "ns_b::ns_c [Namespace]"
      --(f, 29, 21) `defShouldBe` "XXX" -- wrong def
      (f, 32, 10) `defShouldBe` "ns_a [Namespace]"
      (f, 32, 16) `defShouldBe` "ns_a::var_a [VarDecl]"
      (f, 33, 10) `defShouldBe` "ns_b [Namespace]"
      (f, 33, 16) `defShouldBe` "ns_b::var_b [VarDecl]"
      (f, 34, 10) `defShouldBe` "ns_b [Namespace]"
      (f, 34, 16) `defShouldBe` "ns_b::ns_c [Namespace]"
      (f, 34, 22) `defShouldBe` "ns_b::ns_c::var_c [VarDecl]"
      (f, 35, 10) `defShouldBe` "ns_b [Namespace]"
      (f, 35, 16) `defShouldBe` "ns_b::ns_c::var_c [VarDecl]"
      (f, 36, 10) `defShouldBe` "ns_b::var_b [VarDecl]"
      (f, 37, 10) `defShouldBe` "<anonymous>::var_anon [VarDecl]"
      (f, 38, 10) `defShouldBe` "<anonymous>::ns_d [Namespace]"
      (f, 38, 16) `defShouldBe` "<anonymous>::ns_d::var_d [VarDecl]"
      (f, 39, 10) `defShouldBe` "ns_b [Namespace]"
      (f, 39, 16) `defShouldBe` "ns_b::var_b [VarDecl]"
      (f, 40, 10) `defShouldBe` "ns_b [Namespace]"
      (f, 40, 16) `defShouldBe` "ns_b::ns_c [Namespace]"
      (f, 40, 22) `defShouldBe` "ns_b::ns_c::var_c [VarDecl]"
      (f, 41, 10) `defShouldBe` "ns_b [Namespace]"
      (f, 41, 16) `defShouldBe` "ns_b::ns_c::var_c [VarDecl]"
      (f, 42, 10) `defShouldBe` "ns_f [Namespace]"
      (f, 42, 16) `defShouldBe` "ns_f::var_f [VarDecl]"
      (f, 43, 10) `defShouldBe` "ns_f::var_f [VarDecl]"
      (f, 44, 10) `defShouldBe` "ns_a::var_a [VarDecl]"
      (f, 45, 10) `defShouldBe` "ns_b::ns_c::var_c [VarDecl]"
      (f, 46, 10) `defShouldBe` "<anonymous>::ns_d::var_d [VarDecl]"

    it "finds enums" $ let f = "enums.cpp" in do
      index f
      (f, 11, 3) `defShouldBe` "global_enum [EnumDecl]"
      (f, 12, 3) `defShouldBe` "global_enum_class [EnumDecl]"
      (f, 13, 3) `defShouldBe` "main(int, char **)::local_enum [EnumDecl]"
      (f, 14, 3) `defShouldBe` "main(int, char **)::local_enum_class [EnumDecl]"
      (f, 15, 13) `defShouldBe` "main(int, char **)::global_enum_var [VarDecl]"
      (f, 16, 13) `defShouldBe` "global_anonymous_enum_var [VarDecl]"
      (f, 17, 30) `defShouldBe` "main(int, char **)::global_enum_class_var [VarDecl]"
      (f, 18, 13) `defShouldBe` "main(int, char **)::local_enum_var [VarDecl]"
      (f, 19, 13) `defShouldBe` "main(int, char **)::local_anonymous_enum_var [VarDecl]"
      (f, 20, 30) `defShouldBe` "main(int, char **)::local_enum_class_var [VarDecl]"
      (f, 22, 10) `defShouldBe` "global_enum::global_enum_val [EnumConstantDecl]"
      (f, 23, 10) `defShouldBe` "<anonymous>::global_anonymous_enum_val [EnumConstantDecl]"
      (f, 24, 27) `defShouldBe` "global_enum_class [EnumDecl]"
      (f, 24, 46) `defShouldBe` "global_enum_class::global_enum_class_val [EnumConstantDecl]"
      (f, 25, 10) `defShouldBe` "main(int, char **)::local_enum::local_enum_val [EnumConstantDecl]"
      (f, 26, 10) `defShouldBe` "main(int, char **)::<anonymous>::local_anonymous_enum_val [EnumConstantDecl]"
      (f, 27, 27) `defShouldBe` "main(int, char **)::local_enum_class [EnumDecl]"
      (f, 27, 45) `defShouldBe` "main(int, char **)::local_enum_class::local_enum_class_val [EnumConstantDecl]"

    it "finds structs" $ let f = "structs.cpp" in do
      index f
      (f, 12, 10) `defShouldBe` "main(int, char **)::global_struct_var [VarDecl]"
      (f, 12, 28) `defShouldBe` "global_struct::global_struct_val [FieldDecl]"
      (f, 13, 10) `defShouldBe` "global_anonymous_struct_var [VarDecl]"
      (f, 13, 38) `defShouldBe` "<anonymous>::global_anonymous_struct_val [FieldDecl]"
      (f, 14, 10) `defShouldBe` "main(int, char **)::local_struct_var [VarDecl]"
      (f, 14, 27) `defShouldBe` "main(int, char **)::local_struct::local_struct_val [FieldDecl]"
      (f, 15, 10) `defShouldBe` "main(int, char **)::local_anonymous_struct_var [VarDecl]"
      (f, 15, 37) `defShouldBe` "main(int, char **)::<anonymous>::local_anonymous_struct_val [FieldDecl]"

    it "finds bitfields" $ let f = "bitfields.cpp" in do
      index f
      (f, 18, 28) `defShouldBe` "bitfield_struct::bitfield_bool [FieldDecl]"
      (f, 19, 28) `defShouldBe` "bitfield_struct::bitfield_int [FieldDecl]"
      (f, 20, 28) `defShouldBe` "bitfield_struct::bitfield_unsigned_char [FieldDecl]"
      (f, 21, 28) `defShouldBe` "bitfield_struct::nonbitfield [FieldDecl]"
      (f, 22, 28) `defShouldBe` "bitfield_struct::bitfield_enum [FieldDecl]"
      (f, 23, 28) `defShouldBe` "bitfield_struct::bitfield_anonymous_enum [FieldDecl]"
      
    it "finds unions" $ let f = "unions.cpp" in do
      index f
      (f, 27, 3) `defShouldBe` "global_union [UnionDecl]"
      (f, 28, 3) `defShouldBe` "main(int, char **)::local_union [UnionDecl]"
      (f, 30, 10) `defShouldBe` "main(int, char **)::global_union_var [VarDecl]"
      (f, 30, 27) `defShouldBe` "global_union::global_union_val_int [FieldDecl]"
      (f, 31, 27) `defShouldBe` "global_union::global_union_val_char [FieldDecl]"
      (f, 32, 10) `defShouldBe` "global_anonymous_union_var [VarDecl]"
      (f, 32, 37) `defShouldBe` "<anonymous>::global_anonymous_union_val_int [FieldDecl]"
      (f, 33, 37) `defShouldBe` "<anonymous>::global_anonymous_union_val_char [FieldDecl]"
      (f, 34, 10) `defShouldBe` "main(int, char **)::local_union_var [VarDecl]"
      (f, 34, 26) `defShouldBe` "main(int, char **)::local_union::local_union_val_int [FieldDecl]"
      (f, 35, 26) `defShouldBe` "main(int, char **)::local_union::local_union_val_char [FieldDecl]"
      (f, 36, 10) `defShouldBe` "main(int, char **)::local_anonymous_union_var [VarDecl]"
      (f, 36, 36) `defShouldBe` "main(int, char **)::<anonymous>::local_anonymous_union_val_int [FieldDecl]"
      (f, 37, 36) `defShouldBe` "main(int, char **)::<anonymous>::local_anonymous_union_val_char [FieldDecl]"

    it "finds classes" $ let f = "classes.cpp" in do
      index f
      (f, 51, 3) `defShouldBe` "global_class [ClassDecl]"
      --(f, 51, 16) `defShouldBe` "XXX constructors don't work"
      (f, 52, 3) `defShouldBe` "global_class [ClassDecl]"
      --(f, 52, 16) `defShouldBe` "XXX constructors don't work"
      (f, 53, 3) `defShouldBe` "main(int, char **)::local_class [ClassDecl]"
      --(f, 53, 15) `defShouldBe` "XXX constructors don't work"
      (f, 54, 3) `defShouldBe` "global_class [ClassDecl]"
      (f, 54, 17) `defShouldBe` "global_class::nested_class [ClassDecl]"
      --(f, 54, 30) `defShouldBe` "XXX constructors don't work"
      (f, 55, 17) `defShouldBe` "global_class::nested_union [UnionDecl]"
      (f, 56, 17) `defShouldBe` "global_class::nested_enum [EnumDecl]"
      (f, 58, 13) `defShouldBe` "global_class [ClassDecl]"
      (f, 58, 27) `defShouldBe` "global_class::static_method(int) [CXXMethod]"
      (f, 59, 27) `defShouldBe` "global_class::static_field [VarDecl]"
      (f, 60, 27) `defShouldBe` "global_class::nested_class [ClassDecl]"
      (f, 60, 41) `defShouldBe` "global_class::nested_class::nested_static_field [VarDecl]"
      (f, 61, 27) `defShouldBe` "global_class::nested_enum::nested_enum_val [EnumConstantDecl]"
      (f, 63, 10) `defShouldBe` "global_instance [VarDecl]"
      (f, 63, 26) `defShouldBe` "global_class::field [FieldDecl]"
      (f, 64, 26) `defShouldBe` "global_class::method(int) [CXXMethod]"
      (f, 65, 10) `defShouldBe` "main(int, char **)::local_instance [VarDecl]"
      (f, 65, 25) `defShouldBe` "global_class::field [FieldDecl]"
      (f, 66, 25) `defShouldBe` "global_class::method(int) [CXXMethod]"
      (f, 67, 10) `defShouldBe` "main(int, char **)::nested_instance [VarDecl]"
      (f, 67, 26) `defShouldBe` "global_class::nested_class::nested_field [FieldDecl]"
      (f, 68, 26) `defShouldBe` "global_class::nested_class::nested_method(int) [CXXMethod]"
      (f, 69, 10) `defShouldBe` "main(int, char **)::nested_union_var [VarDecl]"
      (f, 69, 27) `defShouldBe` "global_class::nested_union::nested_union_val_int [FieldDecl]"
      (f, 70, 27) `defShouldBe` "global_class::nested_union::nested_union_val_char [FieldDecl]"
      (f, 71, 10) `defShouldBe` "main(int, char **)::local_class_instance [VarDecl]"
      (f, 71, 31) `defShouldBe` "main(int, char **)::local_class::local_field [FieldDecl]"
      (f, 72, 31) `defShouldBe` "main(int, char **)::local_class::local_method(int) [CXXMethod]"
      (f, 73, 10) `defShouldBe` "main(int, char **)::anonymous_instance [VarDecl]"
      (f, 73, 29) `defShouldBe` "main(int, char **)::<anonymous>::anonymous_field [FieldDecl]"
      (f, 74, 29) `defShouldBe` "main(int, char **)::<anonymous>::anonymous_method(int) [CXXMethod]"

    it "finds virtual methods" $ let f = "virtual.cpp" in do
      index f

      -- Instance values.
      (f, 6, 3) `defShouldBe` "AB [ClassDecl]"
      (f, 7, 3) `defShouldBe` "main(int, char **)::AB_instance [VarDecl]"
      (f, 7, 15) `defShouldBe` "AB::A_pure_method() [CXXMethod]"
      (f, 8, 15) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 11, 3) `defShouldBe` "main(int, char **)::ABC_instance [VarDecl]"
      (f, 11, 16) `defShouldBe` "ABC::A_pure_method() [CXXMethod]"
      (f, 12, 16) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      (f, 13, 16) `defShouldBe` "AB [ClassDecl]"
      (f, 13, 20) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 14, 16) `defShouldBe` "ABC [ClassDecl]"
      (f, 14, 21) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"

      -- Pointers to instances.
      (f, 17, 3) `defShouldBe` "A [ClassDecl]"
      (f, 18, 10) `defsShouldBe`
        ["A::A_pure_method() [CXXMethod]",
         "AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 19, 12) `defsShouldBe`
        ["A::A_pure_method() [CXXMethod]",
         "AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 20, 10) `defShouldBe` "A [ClassDecl]"
      (f, 20, 13) `defShouldBe` "A::A_pure_method() [CXXMethod]"
      (f, 21, 15) `defShouldBe` "A::A_pure_method() [CXXMethod]"
      (f, 23, 3) `defShouldBe` "AB [ClassDecl]"
      (f, 24, 11) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 25, 13) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 26, 11) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 27, 13) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 28, 15) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 28, 17) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 31, 3) `defShouldBe` "ABC [ClassDecl]"
      (f, 32, 12) `defShouldBe` "ABC::A_pure_method() [CXXMethod]"
      (f, 33, 14) `defShouldBe` "ABC::A_pure_method() [CXXMethod]"
      (f, 34, 12) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      (f, 35, 14) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      (f, 36, 12) `defShouldBe` "AB [ClassDecl]"
      (f, 36, 16) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 37, 14) `defShouldBe` "AB [ClassDecl]"
      (f, 37, 18) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 38, 17) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      (f, 39, 19) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      (f, 42, 18) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 43, 20) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 44, 18) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 45, 20) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 46, 22) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 47, 24) `defShouldBe` "AB::AB_method(int) [CXXMethod]"

      -- References to instances.
      (f, 50, 3) `defShouldBe` "A [ClassDecl]"
      (f, 51, 18) `defsShouldBe`
        ["A::A_pure_method() [CXXMethod]",
         "AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 52, 18) `defShouldBe` "A [ClassDecl]"
      (f, 52, 21) `defShouldBe` "A::A_pure_method() [CXXMethod]"
      (f, 54, 3) `defShouldBe` "AB [ClassDecl]"
      (f, 55, 19) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 56, 19) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 57, 23) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 59, 3) `defShouldBe` "ABC [ClassDecl]"
      (f, 60, 20) `defShouldBe` "ABC::A_pure_method() [CXXMethod]"
      (f, 61, 20) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      (f, 62, 20) `defShouldBe` "AB [ClassDecl]"
      (f, 62, 24) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 63, 20) `defShouldBe` "ABC [ClassDecl]"
      (f, 63, 25) `defShouldBe` "ABC::AB_method(int) [CXXMethod]"
      (f, 66, 17) `defsShouldBe`
        ["AB::A_pure_method() [CXXMethod]",
         "ABC::A_pure_method() [CXXMethod]",
         "ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 67, 17) `defsShouldBe`
        ["AB::AB_method(int) [CXXMethod]",
         "ABDE::AB_method(int) [CXXMethod]",
         "ABC::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 68, 21) `defShouldBe` "AB::AB_method(int) [CXXMethod]"

    it "finds virtual methods in struct members" $ let f = "virtual-in-structs.cpp" in do
      index f

      -- Structs containing instance values.
      (f, 14, 28) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 15, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 16, 32) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 17, 34) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 20, 24) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 21, 24) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 22, 28) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 23, 30) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 26, 23) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 27, 23) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 28, 27) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 29, 29) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Structs containing pointers to instances.
      (f, 33, 33) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 34, 33) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 35, 37) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 36, 39) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 39, 29) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 40, 29) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 41, 33) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 42, 35) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 45, 28) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 46, 28) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 47, 32) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 48, 34) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Structs containing references to instances.
      (f, 52, 32) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 53, 32) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 54, 36) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 55, 38) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 58, 28) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 59, 28) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 60, 32) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 61, 34) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 64, 27) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 65, 27) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 66, 31) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 67, 33) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

    it "finds virtual methods in return values" $ let f = "virtual-in-return-value.cpp" in do
      index f

      -- Functions returning values or pointers.
      (f, 33, 22) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 34, 22) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 35, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 36, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --(f, 38, 23) `defShouldBe` "XXX" -- Doesn't return all possibilities.
      --(f, 39, 23) `defShouldBe` "XXX"
      (f, 40, 27) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 41, 29) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --(f, 43, 22) `defShouldBe` "XXX"
      --(f, 44, 22) `defShouldBe` "XXX"
      (f, 45, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 46, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Function pointers to functions returning values or pointers.
      --(f, 50, 29) `defShouldBe` "XXX" -- Returning by value but we show multiple defs!
      --(f, 51, 29) `defShouldBe` "XXX"
      (f, 52, 33) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 53, 35) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 56, 30) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 57, 30) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 58, 34) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 59, 36) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 62, 29) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 63, 29) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 64, 33) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 65, 35) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Function references to functions returning values or pointers.
      (f, 69, 22) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 70, 22) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 71, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 72, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --(f, 75, 23) `defShouldBe` "XXX" -- Should see multiple defs.
      --(f, 76, 23) `defShouldBe` "XXX"
      (f, 77, 27) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 78, 29) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --(f, 81, 22) `defShouldBe` "XXX" -- --Should see multiple defs.
      --(f, 82, 22) `defShouldBe` "XXX"
      (f, 83, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 84, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Methods on instances returning values or pointers.
      (f, 88, 48) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 89, 48) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 90, 52) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 91, 54) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --(f, 93, 49) `defShouldBe` "XXX"
      --(f, 94, 49) `defShouldBe` "XXX"
      (f, 95, 53) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 96, 55) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --(f, 98, 48) `defShouldBe` "XXX"
      --(f, 99, 48) `defShouldBe` "XXX"
      (f, 100, 52) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 101, 54) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Methods on pointers to instances returning values or pointers.
      (f, 105, 44) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 106, 44) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 107, 48) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 108, 50) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --(f, 110, 45) `defShouldBe` "XXX"
      --(f, 111, 45) `defShouldBe` "XXX"
      (f, 112, 49) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 113, 51) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --(f, 115, 44) `defShouldBe` "XXX"
      --(f, 116, 44) `defShouldBe` "XXX"
      (f, 117, 48) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 118, 50) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Methods on references to instances returning values or pointers.
      (f, 122, 43) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 123, 43) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 124, 47) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 125, 49) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --(f, 127, 44) `defShouldBe` "XXX"
      --(f, 128, 44) `defShouldBe` "XXX"
      (f, 129, 48) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 130, 50) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      --(f, 132, 43) `defShouldBe` "XXX"
      --(f, 133, 43) `defShouldBe` "XXX"
      (f, 134, 47) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 135, 49) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Invocations of method pointers returning values or pointers.
      (f, 138, 71) `defShouldBe` "ABDE_container::method_ABDE_by_val() [CXXMethod]"
      --(f, 139, 29) `defShouldBe` "XXX" -- wrong def totally
      --(f, 139, 55) `defShouldBe` --"ABDE::A_pure_method() [CXXMethod]" -- too many defs
      --(f, 140, 55) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 141, 59) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 142, 61) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 145, 56) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 146, 56) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 147, 60) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 148, 62) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 151, 55) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 152, 55) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 153, 59) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 154, 61) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

    it "finds virtual methods which some classes don't override" $ let f = "virtual-no-override.cpp" in do
      index f

      -- Classes which don't override a virtual method defined in an ancestor class.
      (f, 7, 16) `defShouldBe` "AB::A_pure_method() [CXXMethod]"
      (f, 8, 16) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 9, 20) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 10, 21) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      --(f, 13, 12) `defShouldBe` "XXX" -- Wrongly includes ABC.
      --(f, 14, 12) `defShouldBe` "XXX"
      (f, 15, 16) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 16, 17) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      --(f, 19, 11) `defShouldBe` "XXX"
      --(f, 20, 11) `defShouldBe` "XXX"
      (f, 21, 15) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 22, 16) `defShouldBe` "AB::AB_method(int) [CXXMethod]"

      -- Classes which have an ancestor which didn't override a method.
      (f, 26, 17) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 27, 17) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 28, 21) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 29, 22) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 30, 23) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 33, 13) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 34, 13) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 35, 17) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 36, 18) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 37, 19) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 40, 12) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 41, 12) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 42, 16) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 43, 17) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 44, 18) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

    -- easy: pointer to method
    -- medium: multiple inheritance, operator overloads, inherited fields and static members
    -- worst case scenario: templates

    -- need to add tests for 'find references', 'bases', 'overrides', etc.
    -- remember to ensure that find references works with macro expansions!
