{-# LANGUAGE QuasiQuotes #-}

module VirtualMethods (testVirtualMethods) where

import Pygmalion.Test
import Pygmalion.Test.TH

testVirtualMethods = runPygmalionTest "virtual.cpp" $ [pygTest|
    #include "virtual.h"

    int main(int argc, char** argv)
    {
      // Instance values.
      @AB AB_instance; ~[Def "AB [ClassDecl]"]~
      @AB_instance.@A_pure_method(); ~[Def "main(int, char **)::AB_instance [VarDecl]",
                                       Def "AB::A_pure_method() [CXXMethod]"]~
      AB_instance.@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~

      ABC ABC_instance;
      @ABC_instance.@A_pure_method(); ~[Def "main(int, char **)::ABC_instance [VarDecl]",
                                        Def "ABC::A_pure_method() [CXXMethod]"]~
      ABC_instance.@AB_method(0); ~[Def "ABC::AB_method(int) [CXXMethod]"]~
      ABC_instance.@AB::@AB_method(0); ~[Def "AB [ClassDecl]",
                                         Def "AB::AB_method(int) [CXXMethod]"]~
      ABC_instance.@ABC::@AB_method(0); ~[Def "ABC [ClassDecl]",
                                          Def "ABC::AB_method(int) [CXXMethod]"]~

      // Pointers to instances.
      @A* A_ptr = new AB; ~[Def "A [ClassDecl]"]~
      A_ptr->@A_pure_method(); ~[Defs ["A::A_pure_method() [CXXMethod]",
                                       "AB::A_pure_method() [CXXMethod]",
                                       "ABC::A_pure_method() [CXXMethod]",
                                       "ABDE::A_pure_method() [CXXMethod]",
                                       "ABDEF::A_pure_method() [CXXMethod]"]]~
      (*A_ptr).@A_pure_method(); ~[Defs ["A::A_pure_method() [CXXMethod]",
                                         "AB::A_pure_method() [CXXMethod]",
                                         "ABC::A_pure_method() [CXXMethod]",
                                         "ABDE::A_pure_method() [CXXMethod]",
                                         "ABDEF::A_pure_method() [CXXMethod]"]]~
      A_ptr->@A::@A_pure_method(); ~[Def "A [ClassDecl]",
                                     Def "A::A_pure_method() [CXXMethod]"]~
      (*A_ptr).A::@A_pure_method(); ~[Def "A::A_pure_method() [CXXMethod]"]~

      @AB* AB_ptr = new AB; ~[Def "AB [ClassDecl]"]~
      AB_ptr->@A_pure_method(); ~[Defs ["AB::A_pure_method() [CXXMethod]",
                                        "ABC::A_pure_method() [CXXMethod]",
                                        "ABDE::A_pure_method() [CXXMethod]",
                                        "ABDEF::A_pure_method() [CXXMethod]"]]~
      (*AB_ptr).@A_pure_method(); ~[Defs ["AB::A_pure_method() [CXXMethod]",
                                          "ABC::A_pure_method() [CXXMethod]",
                                          "ABDE::A_pure_method() [CXXMethod]",
                                          "ABDEF::A_pure_method() [CXXMethod]"]]~
      AB_ptr->@AB_method(0); ~[Defs ["AB::AB_method(int) [CXXMethod]",
                                     "ABDE::AB_method(int) [CXXMethod]",
                                     "ABC::AB_method(int) [CXXMethod]",
                                     "ABDEF::AB_method(int) [CXXMethod]"]]~
      (*AB_ptr).@AB_method(0); ~[Defs ["AB::AB_method(int) [CXXMethod]",
                                       "ABDE::AB_method(int) [CXXMethod]",
                                       "ABC::AB_method(int) [CXXMethod]",
                                       "ABDEF::AB_method(int) [CXXMethod]"]]~
      AB_ptr->AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      (*AB_ptr).AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~

      @ABC* ABC_ptr = new ABC; ~[Def "ABC [ClassDecl]"]~
      ABC_ptr->@A_pure_method(); ~[Def "ABC::A_pure_method() [CXXMethod]"]~
      (*ABC_ptr).@A_pure_method(); ~[Def "ABC::A_pure_method() [CXXMethod]"]~
      ABC_ptr->@AB_method(0); ~[Def "ABC::AB_method(int) [CXXMethod]"]~
      (*ABC_ptr).@AB_method(0); ~[Def "ABC::AB_method(int) [CXXMethod]"]~
      ABC_ptr->@AB::@AB_method(0); ~[Def "AB [ClassDecl]",
                                     Def "AB::AB_method(int) [CXXMethod]"]~
      (*ABC_ptr).@AB::@AB_method(0); ~[Def "AB [ClassDecl]",
                                       Def "AB::AB_method(int) [CXXMethod]"]~
      ABC_ptr->ABC::@AB_method(0); ~[Def "ABC::AB_method(int) [CXXMethod]"]~
      (*ABC_ptr).ABC::@AB_method(0); ~[Def "ABC::AB_method(int) [CXXMethod]"]~

      AB* ABC_in_AB_ptr = ABC_ptr;
      ABC_in_AB_ptr->@A_pure_method(); ~[Defs ["AB::A_pure_method() [CXXMethod]",
                                               "ABC::A_pure_method() [CXXMethod]",
                                               "ABDE::A_pure_method() [CXXMethod]",
                                               "ABDEF::A_pure_method() [CXXMethod]"]]~
      (*ABC_in_AB_ptr).@A_pure_method(); ~[Defs ["AB::A_pure_method() [CXXMethod]",
                                                 "ABC::A_pure_method() [CXXMethod]",
                                                 "ABDE::A_pure_method() [CXXMethod]",
                                                 "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABC_in_AB_ptr->@AB_method(0); ~[Defs ["AB::AB_method(int) [CXXMethod]",
                                            "ABDE::AB_method(int) [CXXMethod]",
                                            "ABC::AB_method(int) [CXXMethod]",
                                            "ABDEF::AB_method(int) [CXXMethod]"]]~
      (*ABC_in_AB_ptr).@AB_method(0); ~[Defs ["AB::AB_method(int) [CXXMethod]",
                                              "ABDE::AB_method(int) [CXXMethod]",
                                              "ABC::AB_method(int) [CXXMethod]",
                                              "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABC_in_AB_ptr->AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      (*ABC_in_AB_ptr).AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~

      // References to instances.
      @A& A_instance_ref = AB_instance; ~[Def "A [ClassDecl]"]~
      A_instance_ref.@A_pure_method(); ~[Defs ["A::A_pure_method() [CXXMethod]",
                                               "AB::A_pure_method() [CXXMethod]",
                                               "ABC::A_pure_method() [CXXMethod]",
                                               "ABDE::A_pure_method() [CXXMethod]",
                                               "ABDEF::A_pure_method() [CXXMethod]"]]~
      A_instance_ref.@A::@A_pure_method(); ~[Def "A [ClassDecl]",
                                             Def "A::A_pure_method() [CXXMethod]"]~

      @AB& AB_instance_ref = AB_instance; ~[Def "AB [ClassDecl]"]~
      AB_instance_ref.@A_pure_method(); ~[Defs ["AB::A_pure_method() [CXXMethod]",
                                                "ABC::A_pure_method() [CXXMethod]",
                                                "ABDE::A_pure_method() [CXXMethod]",
                                                "ABDEF::A_pure_method() [CXXMethod]"]]~
      AB_instance_ref.@AB_method(0); ~[Defs ["AB::AB_method(int) [CXXMethod]",
                                             "ABDE::AB_method(int) [CXXMethod]",
                                             "ABC::AB_method(int) [CXXMethod]",
                                             "ABDEF::AB_method(int) [CXXMethod]"]]~
      AB_instance_ref.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~

      @ABC& ABC_instance_ref = ABC_instance; ~[Def "ABC [ClassDecl]"]~
      ABC_instance_ref.@A_pure_method(); ~[Def "ABC::A_pure_method() [CXXMethod]"]~
      ABC_instance_ref.@AB_method(0); ~[Def "ABC::AB_method(int) [CXXMethod]"]~
      ABC_instance_ref.@AB::@AB_method(0); ~[Def "AB [ClassDecl]",
                                             Def "AB::AB_method(int) [CXXMethod]"]~
      ABC_instance_ref.@ABC::@AB_method(0); ~[Def "ABC [ClassDecl]",
                                              Def "ABC::AB_method(int) [CXXMethod]"]~

      AB& ABC_in_AB_ref = ABC_instance;
      ABC_in_AB_ref.@A_pure_method(); ~[Defs ["AB::A_pure_method() [CXXMethod]",
                                              "ABC::A_pure_method() [CXXMethod]",
                                              "ABDE::A_pure_method() [CXXMethod]",
                                              "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABC_in_AB_ref.@AB_method(0); ~[Defs ["AB::AB_method(int) [CXXMethod]",
                                           "ABDE::AB_method(int) [CXXMethod]",
                                           "ABC::AB_method(int) [CXXMethod]",
                                           "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABC_in_AB_ref.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~

      return 0;
    }
  |]
