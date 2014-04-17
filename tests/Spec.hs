import Control.Concurrent
import System.Directory
import Test.Hspec

import Pygmalion.Test (defShouldBe, defsShouldBe, index, line,
                       runPygmalionTest, Test(..), test, withPygd)

import ForwardDeclarations
import Macros
import TemplateFunctions
import VirtualMethods
import VirtualMethodsInStructs
import VirtualMethodsWithNoOverride

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

    it "finds macros" testMacros

    it "finds typedefs" $ let f = "typedefs.cpp" in do
      index f
      (f, 6, 8) `defShouldBe` "clazz::clazz_typedef [TypedefDecl]"
      (f, 6, 30) `defShouldBe` "ns::ns_typedef [TypedefDecl]"
      (f, 6, 44) `defShouldBe` "global_typedef [TypedefDecl]"
      (f, 12, 26) `defShouldBe` "clazz::clazz_typedef [TypedefDecl]"
      --(f, 12, 42) `defShouldBe` "templat<clazz::clazz_typedef>::templat_typedef [TypedefDecl]" -- libclang bug
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
      --(f, 9, 15) `defShouldBe` "XXX" -- wrong def -- libclang bug
      (f, 12, 7) `defShouldBe` "ns_b [Namespace]"
      --(f, 12, 13) `defShouldBe` "XXX" -- no def  -- libclang bug
      (f, 20, 18) `defShouldBe` "ns_b [Namespace]"
      (f, 24, 17) `defShouldBe` "ns_f [Namespace]"
      (f, 29, 9) `defShouldBe` "ns_b [Namespace]"
      (f, 29, 15) `defShouldBe` "ns_b::ns_c [Namespace]"
      --(f, 29, 21) `defShouldBe` "XXX" -- wrong def  -- libclang bug
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
      (f, 51, 16) `defShouldBe` "global_class::global_class() [Constructor]"
      (f, 52, 3) `defShouldBe` "global_class [ClassDecl]"
      (f, 52, 16) `defShouldBe` "global_class::global_class(int) [Constructor]"
      (f, 53, 3) `defShouldBe` "main(int, char **)::local_class [ClassDecl]"
      (f, 53, 15) `defShouldBe` "main(int, char **)::local_class_instance [VarDecl]"
      (f, 54, 3) `defShouldBe` "global_class [ClassDecl]"
      (f, 54, 17) `defShouldBe` "global_class::nested_class [ClassDecl]"
      (f, 54, 30) `defShouldBe` "main(int, char **)::nested_instance [VarDecl]"
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

    it "finds virtual methods" testVirtualMethods

    it "finds virtual methods in struct members" testVirtualMethodsInStructs

    it "finds virtual methods in return values" $ let f = "virtual-in-return-value.cpp" in do
      index f

      -- Functions returning values or pointers.
      (f, 33, 22) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 34, 22) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 35, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 36, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 38, 23) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 39, 23) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 40, 27) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 41, 29) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 43, 22) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 44, 22) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 45, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 46, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Function pointers to functions returning values or pointers.
      (f, 50, 26) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 51, 26) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 52, 30) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 53, 32) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 56, 27) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 57, 27) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 58, 31) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 59, 33) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 62, 26) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 63, 26) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 64, 30) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 65, 32) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Function references to functions returning values or pointers.
      (f, 69, 22) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 70, 22) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 71, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 72, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 75, 23) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 76, 23) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 77, 27) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 78, 29) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 81, 22) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 82, 22) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 83, 26) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 84, 28) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Methods on instances returning values or pointers.
      (f, 88, 48) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 89, 48) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 90, 52) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 91, 54) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 93, 49) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 94, 49) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 95, 53) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 96, 55) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 98, 48) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 99, 48) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 100, 52) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 101, 54) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Methods on pointers to instances returning values or pointers.
      (f, 105, 44) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 106, 44) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 107, 48) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 108, 50) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 110, 45) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 111, 45) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 112, 49) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 113, 51) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 115, 44) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 116, 44) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 117, 48) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 118, 50) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Methods on references to instances returning values or pointers.
      (f, 122, 43) `defShouldBe` "ABDE::A_pure_method() [CXXMethod]"
      (f, 123, 43) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 124, 47) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 125, 49) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 127, 44) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 128, 44) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 129, 48) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 130, 50) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"
      (f, 132, 43) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 133, 43) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
      (f, 134, 47) `defShouldBe` "AB::AB_method(int) [CXXMethod]"
      (f, 135, 49) `defShouldBe` "ABDE::AB_method(int) [CXXMethod]"

      -- Invocations of method pointers returning values or pointers.
      (f, 138, 71) `defShouldBe` "ABDE_container::method_ABDE_by_val() [CXXMethod]"
      (f, 139, 29) `defShouldBe` "main(int, char **)::method_ptr_ABDE_by_val [VarDecl]"
      (f, 139, 55) `defsShouldBe`
        ["ABDE::A_pure_method() [CXXMethod]",
         "ABDEF::A_pure_method() [CXXMethod]"]
      (f, 140, 55) `defsShouldBe`
        ["ABDE::AB_method(int) [CXXMethod]",
         "ABDEF::AB_method(int) [CXXMethod]"]
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

    it "finds virtual methods which some classes don't override" $
       testVirtualMethodsWithNoOverride

    it "finds method pointers" $ let f = "method-pointers.cpp" in do
      index f

      -- Top-level base class.
      (f, 23, 35) `defShouldBe` "clazz [ClassDecl]"
      (f, 23, 42) `defShouldBe` "clazz::static_method(int) [CXXMethod]"
      (f, 24, 3) `defShouldBe` "main(int, char **)::static_method_ptr [VarDecl]"
      --(f, 26, 8) `defShouldBe` "clazz [ClassDecl]" -- libclang bug
      (f, 26, 36) `defShouldBe` "clazz [ClassDecl]"
      (f, 26, 43) `defShouldBe` "clazz::method(int) [CXXMethod]"
      (f, 27, 4) `defShouldBe` "main(int, char **)::clazz_instance [VarDecl]"
      (f, 27, 20) `defShouldBe` "main(int, char **)::method_ptr [VarDecl]"
      (f, 28, 4) `defShouldBe` "main(int, char **)::clazz_ptr [VarDecl]"
      (f, 28, 16) `defShouldBe` "main(int, char **)::method_ptr [VarDecl]"
      (f, 29, 4) `defShouldBe` "main(int, char **)::clazz_ref [VarDecl]"
      (f, 29, 15) `defShouldBe` "main(int, char **)::method_ptr [VarDecl]"
      -- (f, 31, 8) `defShouldBe` "clazz [ClassDecl]" -- libclang bug
      (f, 31, 44) `defShouldBe` "clazz [ClassDecl]"
      (f, 31, 51) `defShouldBe` "clazz::virtual_method(int) [CXXMethod]"
      (f, 32, 20) `defShouldBe` "main(int, char **)::virtual_method_ptr [VarDecl]"
      (f, 33, 16) `defShouldBe` "main(int, char **)::virtual_method_ptr [VarDecl]"
      (f, 34, 15) `defShouldBe` "main(int, char **)::virtual_method_ptr [VarDecl]"
      (f, 41, 42) `defShouldBe` "clazz [ClassDecl]"
      (f, 41, 49) `defShouldBe` "clazz::nested_clazz [ClassDecl]"
      (f, 41, 63) `defShouldBe` "clazz::nested_clazz::static_nested_method(int) [CXXMethod]"
      (f, 42, 3) `defShouldBe` "main(int, char **)::static_nested_method_ptr [VarDecl]"
      -- (f, 44, 8) `defShouldBe` "clazz [ClassDecl]" -- libclang bug
      -- (f, 44, 15) `defShouldBe` "clazz::nested_clazz [ClassDecl]" -- libclang bug
      (f, 44, 57) `defShouldBe` "clazz [ClassDecl]"
      (f, 44, 67) `defShouldBe` "clazz::nested_clazz [ClassDecl]"
      (f, 44, 78) `defShouldBe` "clazz::nested_clazz::nested_method(int) [CXXMethod]"
      (f, 45, 27) `defShouldBe` "main(int, char **)::nested_method_ptr [VarDecl]"
      (f, 46, 23) `defShouldBe` "main(int, char **)::nested_method_ptr [VarDecl]"
      (f, 47, 22) `defShouldBe` "main(int, char **)::nested_method_ptr [VarDecl]"
      

    it "finds template functions" testTemplateFunctions

    it "doesn't confuse forward declarations with definitions" testForwardDeclarations
        
    -- easy: labels
    -- medium: multiple inheritance, operator overloads, inherited fields and static members
    -- worst case scenario: templates

    -- TODO: add a test that we avoid returning conversion operators
    -- for get definition when possible.

    -- need to add tests for 'find references', 'bases', 'overrides', etc.
    -- remember to ensure that find references works with macro expansions!
