module VirtualMethodsInStructs (testVirtualMethodsInStructs) where

import Pygmalion.Test

testVirtualMethodsInStructs = runPygmalionTest "virtual-in-structs.cpp" $ do
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
