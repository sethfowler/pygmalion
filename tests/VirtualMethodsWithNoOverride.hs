module VirtualMethodsWithNoOverride (testVirtualMethodsWithNoOverride) where

import Pygmalion.Test

testVirtualMethodsWithNoOverride = runPygmalionTest "virtual-no-override.cpp" $ do
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
  test "  ABD_ptr->@A_pure_method();"
       [Defs ["AB::A_pure_method() [CXXMethod]",
              "ABDE::A_pure_method() [CXXMethod]",
              "ABDEF::A_pure_method() [CXXMethod]"]]
  test "  ABD_ptr->@AB_method(0);"
       [Defs ["AB::AB_method(int) [CXXMethod]",
              "ABDE::AB_method(int) [CXXMethod]",
              "ABDEF::AB_method(int) [CXXMethod]"]]
  test "  ABD_ptr->AB::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
  test "  ABD_ptr->ABD::@AB_method(0);" [Def "AB::AB_method(int) [CXXMethod]"]
  line ""
  line "  ABD& ABD_ref = ABD_instance;"
  test "  ABD_ref.@A_pure_method();"
       [Defs ["AB::A_pure_method() [CXXMethod]",
              "ABDE::A_pure_method() [CXXMethod]",
              "ABDEF::A_pure_method() [CXXMethod]"]]
  test "  ABD_ref.@AB_method(0);"
       [Defs ["AB::AB_method(int) [CXXMethod]",
              "ABDE::AB_method(int) [CXXMethod]",
              "ABDEF::AB_method(int) [CXXMethod]"]]
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
