import Control.Concurrent
import System.Directory
import Test.Hspec

import Pygmalion.Test (defShouldBe, defsShouldBe, index, line,
                       runPygmalionTest, Test(..), test, withPygd)

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

    it "finds macros" $ runPygmalionTest "macros.cpp" $ do
      line "#define VAR 0"
      line "#define VARF(x) (int) x"
      line ""
      line "int main(int argc, char** argv)"
      line "{"
      line "  char local_var = 0;"
      line ""
      test "  return @VAR"               [Def "VAR [MacroDefinition]"]
      test "       + @VARF(@local_var);" [Def "VARF [MacroDefinition]", Fails "#105"]
      line "}"

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

    it "finds virtual methods" $ runPygmalionTest "virtual.cpp" $ do

      line "#include \"virtual.h\""
      line ""
      line "int main(int argc, char** argv)"
      line "{"
      line "  // Instance values."
      test "  @AB AB_instance;" [Def "AB [ClassDecl]"]
      test "  @AB_instance.@A_pure_method();" [Def "main(int, char **)::AB_instance [VarDecl]",
                                               Def "AB::A_pure_method() [CXXMethod]"]
      test "  AB_instance.@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABC ABC_instance;"
      test "  @ABC_instance.@A_pure_method();" [Def "main(int, char **)::ABC_instance [VarDecl]",
                                                Def "ABC::A_pure_method() [CXXMethod]"]
      test "  ABC_instance.@AB_method(0);" [Def "ABC::AB_method(int) [CXXMethod]"]
      test "  ABC_instance.@AB::@AB_method(0);" [Def "AB [ClassDecl]",
                                                 Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABC_instance.@ABC::@AB_method(0);" [Def "ABC [ClassDecl]",
                                                  Def "ABC::AB_method(int) [CXXMethod]"]
      line ""
      line "  // Pointers to instances."
      test "  @A* A_ptr = new AB;" [Def "A [ClassDecl]"]
      test "  A_ptr->@A_pure_method();" [Defs ["A::A_pure_method() [CXXMethod]",
                                               "AB::A_pure_method() [CXXMethod]",
                                               "ABC::A_pure_method() [CXXMethod]",
                                               "ABDE::A_pure_method() [CXXMethod]",
                                               "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  (*A_ptr).@A_pure_method();" [Defs ["A::A_pure_method() [CXXMethod]",
                                                 "AB::A_pure_method() [CXXMethod]",
                                                 "ABC::A_pure_method() [CXXMethod]",
                                                 "ABDE::A_pure_method() [CXXMethod]",
                                                 "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  A_ptr->@A::@A_pure_method();" [Def "A [ClassDecl]",
                                             Def "A::A_pure_method() [CXXMethod]"]
      test "  (*A_ptr).A::@A_pure_method();" [Def "A::A_pure_method() [CXXMethod]"]
      line ""
      test "  @AB* AB_ptr = new AB;" [Def "AB [ClassDecl]"]
      test "  AB_ptr->@A_pure_method();" [Defs ["AB::A_pure_method() [CXXMethod]",
                                                "ABC::A_pure_method() [CXXMethod]",
                                                "ABDE::A_pure_method() [CXXMethod]",
                                                "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  (*AB_ptr).@A_pure_method();" [Defs ["AB::A_pure_method() [CXXMethod]",
                                                  "ABC::A_pure_method() [CXXMethod]",
                                                  "ABDE::A_pure_method() [CXXMethod]",
                                                  "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  AB_ptr->@AB_method(0);" [Defs ["AB::AB_method(int) [CXXMethod]",
                                             "ABDE::AB_method(int) [CXXMethod]",
                                             "ABC::AB_method(int) [CXXMethod]",
                                             "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  (*AB_ptr).@AB_method(0);" [Defs ["AB::AB_method(int) [CXXMethod]",
                                               "ABDE::AB_method(int) [CXXMethod]",
                                               "ABC::AB_method(int) [CXXMethod]",
                                               "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  AB_ptr->AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  (*AB_ptr).AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      line ""
      test "  @ABC* ABC_ptr = new ABC;" [Def "ABC [ClassDecl]"]
      test "  ABC_ptr->@A_pure_method();" [Def "ABC::A_pure_method() [CXXMethod]"]
      test "  (*ABC_ptr).@A_pure_method();" [Def "ABC::A_pure_method() [CXXMethod]"]
      test "  ABC_ptr->@AB_method(0);" [Def "ABC::AB_method(int) [CXXMethod]"]
      test "  (*ABC_ptr).@AB_method(0);" [Def "ABC::AB_method(int) [CXXMethod]"]
      test "  ABC_ptr->@AB::@AB_method(0);" [Def "AB [ClassDecl]",
                                             Def "AB::AB_method(int) [CXXMethod]"]
      test "  (*ABC_ptr).@AB::@AB_method(0);" [Def "AB [ClassDecl]",
                                               Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABC_ptr->ABC::@AB_method(0);" [Def "ABC::AB_method(int) [CXXMethod]"]
      test "  (*ABC_ptr).ABC::@AB_method(0);" [Def "ABC::AB_method(int) [CXXMethod]"]
      line ""
      line "  AB* ABC_in_AB_ptr = ABC_ptr;"
      test "  ABC_in_AB_ptr->@A_pure_method();" [Defs ["AB::A_pure_method() [CXXMethod]",
                                                       "ABC::A_pure_method() [CXXMethod]",
                                                       "ABDE::A_pure_method() [CXXMethod]",
                                                       "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  (*ABC_in_AB_ptr).@A_pure_method();" [Defs ["AB::A_pure_method() [CXXMethod]",
                                                         "ABC::A_pure_method() [CXXMethod]",
                                                         "ABDE::A_pure_method() [CXXMethod]",
                                                         "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  ABC_in_AB_ptr->@AB_method(0);" [Defs ["AB::AB_method(int) [CXXMethod]",
                                                    "ABDE::AB_method(int) [CXXMethod]",
                                                    "ABC::AB_method(int) [CXXMethod]",
                                                    "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  (*ABC_in_AB_ptr).@AB_method(0);" [Defs ["AB::AB_method(int) [CXXMethod]",
                                                      "ABDE::AB_method(int) [CXXMethod]",
                                                      "ABC::AB_method(int) [CXXMethod]",
                                                      "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  ABC_in_AB_ptr->AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  (*ABC_in_AB_ptr).AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      line ""
      line "  // References to instances."
      test "  @A& A_instance_ref = AB_instance;" [Def "A [ClassDecl]"]
      test "  A_instance_ref.@A_pure_method();" [Defs ["A::A_pure_method() [CXXMethod]",
                                                       "AB::A_pure_method() [CXXMethod]",
                                                       "ABC::A_pure_method() [CXXMethod]",
                                                       "ABDE::A_pure_method() [CXXMethod]",
                                                       "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  A_instance_ref.@A::@A_pure_method();" [Def "A [ClassDecl]",
                                                     Def "A::A_pure_method() [CXXMethod]"]
      line ""
      test "  @AB& AB_instance_ref = AB_instance;" [Def "AB [ClassDecl]"]
      test "  AB_instance_ref.@A_pure_method();" [Defs ["AB::A_pure_method() [CXXMethod]",
                                                        "ABC::A_pure_method() [CXXMethod]",
                                                        "ABDE::A_pure_method() [CXXMethod]",
                                                        "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  AB_instance_ref.@AB_method(0);" [Defs ["AB::AB_method(int) [CXXMethod]",
                                                     "ABDE::AB_method(int) [CXXMethod]",
                                                     "ABC::AB_method(int) [CXXMethod]",
                                                     "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  AB_instance_ref.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      line ""
      test "  @ABC& ABC_instance_ref = ABC_instance;" [Def "ABC [ClassDecl]"]
      test "  ABC_instance_ref.@A_pure_method();" [Def "ABC::A_pure_method() [CXXMethod]"]
      test "  ABC_instance_ref.@AB_method(0);" [Def "ABC::AB_method(int) [CXXMethod]"]
      test "  ABC_instance_ref.@AB::@AB_method(0);" [Def "AB [ClassDecl]",
                                                     Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABC_instance_ref.@ABC::@AB_method(0);" [Def "ABC [ClassDecl]",
                                                      Def "ABC::AB_method(int) [CXXMethod]"]
      line ""
      line "  AB& ABC_in_AB_ref = ABC_instance;"
      test "  ABC_in_AB_ref.@A_pure_method();" [Defs ["AB::A_pure_method() [CXXMethod]",
                                                      "ABC::A_pure_method() [CXXMethod]",
                                                      "ABDE::A_pure_method() [CXXMethod]",
                                                      "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  ABC_in_AB_ref.@AB_method(0);" [Defs ["AB::AB_method(int) [CXXMethod]",
                                                   "ABDE::AB_method(int) [CXXMethod]",
                                                   "ABC::AB_method(int) [CXXMethod]",
                                                   "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  ABC_in_AB_ref.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      line ""
      line "  return 0;"
      line "}"

    it "finds virtual methods in struct members" $ runPygmalionTest "virtual-in-structs.cpp" $ do
      line "#include \"virtual.h\""
      line ""
      line "struct ABDE_struct { ABDE var; };"
      line "struct ABDE_ptr_struct { ABDE* ptr; };"
      line "struct ABDE_ref_struct { ABDE& ref; };"
      line ""
      line "int main(int argc, char** argv)"
      line "{"
      line "  ABDE ABDE_instance;"
      line "  ABDE* ABDE_ptr = &ABDE_instance;"
      line ""
      line "  // Structs containing instance values."
      line "  ABDE_struct ABDE_struct_instance;"
      test "  ABDE_struct_instance.var.@A_pure_method();" [Def "ABDE::A_pure_method() [CXXMethod]"]
      test "  ABDE_struct_instance.var.@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      test "  ABDE_struct_instance.var.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_struct_instance.var.ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABDE_struct* ABDE_struct_ptr = &ABDE_struct_instance;"
      test "  ABDE_struct_ptr->var.@A_pure_method();" [Def "ABDE::A_pure_method() [CXXMethod]"]
      test "  ABDE_struct_ptr->var.@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      test "  ABDE_struct_ptr->var.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_struct_ptr->var.ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABDE_struct& ABDE_struct_ref = ABDE_struct_instance;"
      test "  ABDE_struct_ref.var.@A_pure_method();" [Def "ABDE::A_pure_method() [CXXMethod]"]
      test "  ABDE_struct_ref.var.@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      test "  ABDE_struct_ref.var.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_struct_ref.var.ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  // Structs containing pointers to instances."
      line "  ABDE_ptr_struct ABDE_ptr_struct_instance { ABDE_ptr };"
      test "  ABDE_ptr_struct_instance.ptr->@A_pure_method();"
           [Defs ["ABDE::A_pure_method() [CXXMethod]",
                  "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  ABDE_ptr_struct_instance.ptr->@AB_method(0);"
           [Defs ["ABDE::AB_method(int) [CXXMethod]",
                  "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  ABDE_ptr_struct_instance.ptr->AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_ptr_struct_instance.ptr->ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABDE_ptr_struct* ABDE_ptr_struct_ptr = &ABDE_ptr_struct_instance;"
      test "  ABDE_ptr_struct_ptr->ptr->@A_pure_method();"
           [Defs ["ABDE::A_pure_method() [CXXMethod]",
                  "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  ABDE_ptr_struct_ptr->ptr->@AB_method(0);"
           [Defs ["ABDE::AB_method(int) [CXXMethod]",
                  "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  ABDE_ptr_struct_ptr->ptr->AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_ptr_struct_ptr->ptr->ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABDE_ptr_struct& ABDE_ptr_struct_ref = ABDE_ptr_struct_instance;"
      test "  ABDE_ptr_struct_ref.ptr->@A_pure_method();"
           [Defs ["ABDE::A_pure_method() [CXXMethod]",
                  "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  ABDE_ptr_struct_ref.ptr->@AB_method(0);"
           [Defs ["ABDE::AB_method(int) [CXXMethod]",
                  "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  ABDE_ptr_struct_ref.ptr->AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_ptr_struct_ref.ptr->ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  // Structs containing references to instances."
      line "  ABDE_ref_struct ABDE_ref_struct_instance { ABDE_instance };"
      test "  ABDE_ref_struct_instance.ref.@A_pure_method();"
           [Defs ["ABDE::A_pure_method() [CXXMethod]",
                  "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  ABDE_ref_struct_instance.ref.@AB_method(0);"
           [Defs ["ABDE::AB_method(int) [CXXMethod]",
                  "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  ABDE_ref_struct_instance.ref.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_ref_struct_instance.ref.ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABDE_ref_struct* ABDE_ref_struct_ptr = &ABDE_ref_struct_instance;"
      test "  ABDE_ref_struct_ptr->ref.@A_pure_method();"
           [Defs ["ABDE::A_pure_method() [CXXMethod]",
                  "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  ABDE_ref_struct_ptr->ref.@AB_method(0);"
           [Defs ["ABDE::AB_method(int) [CXXMethod]",
                  "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  ABDE_ref_struct_ptr->ref.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_ref_struct_ptr->ref.ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABDE_ref_struct& ABDE_ref_struct_ref = ABDE_ref_struct_instance;"
      test "  ABDE_ref_struct_ref.ref.@A_pure_method();"
           [Defs ["ABDE::A_pure_method() [CXXMethod]",
                  "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  ABDE_ref_struct_ref.ref.@AB_method(0);"
           [Defs ["ABDE::AB_method(int) [CXXMethod]",
                  "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  ABDE_ref_struct_ref.ref.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_ref_struct_ref.ref.ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  return 0;"
      line "}"

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

    it "finds virtual methods which some classes don't override" $ runPygmalionTest "virtual-no-override.cpp" $ do

      line "#include \"virtual.h\""
      line ""
      line "int main(int argc, char** argv)"
      line "{"
      line "  // Classes which don't override a virtual method defined in an ancestor class."
      line "  ABD ABD_instance;"
      test "  ABD_instance.@A_pure_method();" [Def "AB::A_pure_method() [CXXMethod]"]
      test "  ABD_instance.@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABD_instance.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABD_instance.ABD::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABD* ABD_ptr;"
      test "  ABD_ptr->@A_pure_method();" [Fails "#12 - Wrongly includes ABC"]
      test "  ABD_ptr->@AB_method(0);" [Fails "#12 - Wrongly includes ABC"]
      test "  ABD_ptr->AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABD_ptr->ABD::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABD& ABD_ref = ABD_instance;"
      test "  ABD_ref.@A_pure_method();" [Fails "#12 - Wrongly includes ABC"]
      test "  ABD_ref.@AB_method(0);" [Fails "#12 - Wrongly includes ABC"]
      test "  ABD_ref.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABD_ref.ABD::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      line ""
      line "  // Classes which have an ancestor which didn't override a method."
      line "  ABDE ABDE_instance;"
      test "  ABDE_instance.@A_pure_method();" [Def "ABDE::A_pure_method() [CXXMethod]"]
      test "  ABDE_instance.@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      test "  ABDE_instance.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_instance.ABD::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_instance.ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABDE* ABDE_ptr;"
      test "  ABDE_ptr->@A_pure_method();"
           [Defs ["ABDE::A_pure_method() [CXXMethod]",
                  "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  ABDE_ptr->@AB_method(0);"
           [Defs ["ABDE::AB_method(int) [CXXMethod]",
                  "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  ABDE_ptr->AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_ptr->ABD::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_ptr->ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  ABDE& ABDE_ref = ABDE_instance;"
      test "  ABDE_ref.@A_pure_method();"
           [Defs ["ABDE::A_pure_method() [CXXMethod]",
                  "ABDEF::A_pure_method() [CXXMethod]"]]
      test "  ABDE_ref.@AB_method(0);"
           [Defs ["ABDE::AB_method(int) [CXXMethod]",
                  "ABDEF::AB_method(int) [CXXMethod]"]]
      test "  ABDE_ref.AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_ref.ABD::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
      test "  ABDE_ref.ABDE::@AB_method(0);" [Def "ABDE::AB_method(int) [CXXMethod]"]
      line ""
      line "  return 0;"
      line "}"

    it "finds method pointers" $ let f = "method-pointers.cpp" in do
      index f

      -- Top-level base class.
      (f, 23, 35) `defShouldBe` "clazz [ClassDecl]"
      (f, 23, 42) `defShouldBe` "clazz::static_method(int) [CXXMethod]"
      (f, 24, 3) `defShouldBe` "main(int, char **)::static_method_ptr [VarDecl]"
      --(f, 26, 8) `defShouldBe` "clazz [ClassDecl]" -- no def
      (f, 26, 36) `defShouldBe` "clazz [ClassDecl]"
      (f, 26, 43) `defShouldBe` "clazz::method(int) [CXXMethod]"
      (f, 27, 4) `defShouldBe` "main(int, char **)::clazz_instance [VarDecl]"
      (f, 27, 20) `defShouldBe` "main(int, char **)::method_ptr [VarDecl]"
      (f, 28, 4) `defShouldBe` "main(int, char **)::clazz_ptr [VarDecl]"
      (f, 28, 16) `defShouldBe` "main(int, char **)::method_ptr [VarDecl]"
      (f, 29, 4) `defShouldBe` "main(int, char **)::clazz_ref [VarDecl]"
      (f, 29, 15) `defShouldBe` "main(int, char **)::method_ptr [VarDecl]"
      -- (f, 31, 8) `defShouldBe` "clazz [ClassDecl]" -- no def
      (f, 31, 44) `defShouldBe` "clazz [ClassDecl]"
      (f, 31, 51) `defShouldBe` "clazz::virtual_method(int) [CXXMethod]"
      (f, 32, 20) `defShouldBe` "main(int, char **)::virtual_method_ptr [VarDecl]"
      (f, 33, 16) `defShouldBe` "main(int, char **)::virtual_method_ptr [VarDecl]"
      (f, 34, 15) `defShouldBe` "main(int, char **)::virtual_method_ptr [VarDecl]"
      (f, 41, 42) `defShouldBe` "clazz [ClassDecl]"
      (f, 41, 49) `defShouldBe` "clazz::nested_clazz [ClassDecl]"
      (f, 41, 63) `defShouldBe` "clazz::nested_clazz::static_nested_method(int) [CXXMethod]"
      (f, 42, 3) `defShouldBe` "main(int, char **)::static_nested_method_ptr [VarDecl]"
      -- (f, 44, 8) `defShouldBe` "clazz [ClassDecl]" -- no def
      -- (f, 44, 15) `defShouldBe` "clazz::nested_clazz [ClassDecl]" -- no def
      (f, 44, 57) `defShouldBe` "clazz [ClassDecl]"
      (f, 44, 67) `defShouldBe` "clazz::nested_clazz [ClassDecl]"
      (f, 44, 78) `defShouldBe` "clazz::nested_clazz::nested_method(int) [CXXMethod]"
      (f, 45, 27) `defShouldBe` "main(int, char **)::nested_method_ptr [VarDecl]"
      (f, 46, 23) `defShouldBe` "main(int, char **)::nested_method_ptr [VarDecl]"
      (f, 47, 22) `defShouldBe` "main(int, char **)::nested_method_ptr [VarDecl]"
      

        
    -- easy: labels
    -- medium: multiple inheritance, operator overloads, inherited fields and static members
    -- worst case scenario: templates

    -- need to add tests for 'find references', 'bases', 'overrides', etc.
    -- remember to ensure that find references works with macro expansions!
