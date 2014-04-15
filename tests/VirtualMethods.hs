module VirtualMethods (testVirtualMethods) where

import Pygmalion.Test

testVirtualMethods = runPygmalionTest "virtual.cpp" $ do
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
