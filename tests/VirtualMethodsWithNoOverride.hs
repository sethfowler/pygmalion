{-# LANGUAGE QuasiQuotes #-}

module VirtualMethodsWithNoOverride (testVirtualMethodsWithNoOverride) where

import Pygmalion.Test
import Pygmalion.Test.TH

testVirtualMethodsWithNoOverride = runPygmalionTest "virtual-no-override.cpp" $ [pygTest|
    #include "virtual.h"

    int main(int argc, char** argv)
    {
      // Classes which don't override a virtual method defined in an ancestor class.
      ABD ABD_instance;
      ABD_instance.@A_pure_method(); ~[Def "AB::A_pure_method() [CXXMethod]"]~
      ABD_instance.@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABD_instance.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABD_instance.ABD::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~

      ABD* ABD_ptr;
      ABD_ptr->@A_pure_method(); ~~
         ~[Defs ["AB::A_pure_method() [CXXMethod]",
                 "ABDE::A_pure_method() [CXXMethod]",
                 "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABD_ptr->@AB_method(0); ~~
         ~[Defs ["AB::AB_method(int) [CXXMethod]",
                 "ABDE::AB_method(int) [CXXMethod]",
                 "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABD_ptr->AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABD_ptr->ABD::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~

      ABD& ABD_ref = ABD_instance;
      ABD_ref.@A_pure_method(); ~~
         ~[Defs ["AB::A_pure_method() [CXXMethod]",
                 "ABDE::A_pure_method() [CXXMethod]",
                 "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABD_ref.@AB_method(0); ~~
         ~[Defs ["AB::AB_method(int) [CXXMethod]",
                 "ABDE::AB_method(int) [CXXMethod]",
                 "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABD_ref.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABD_ref.ABD::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~

      // Classes which have an ancestor which didn't override a method.
      ABDE ABDE_instance;
      ABDE_instance.@A_pure_method(); ~[Def "ABDE::A_pure_method() [CXXMethod]"]~
      ABDE_instance.@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
      ABDE_instance.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_instance.ABD::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_instance.ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~

      ABDE* ABDE_ptr;
      ABDE_ptr->@A_pure_method(); ~~
         ~[Defs ["ABDE::A_pure_method() [CXXMethod]",
                 "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABDE_ptr->@AB_method(0); ~~
         ~[Defs ["ABDE::AB_method(int) [CXXMethod]",
                 "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABDE_ptr->AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_ptr->ABD::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_ptr->ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~

      ABDE& ABDE_ref = ABDE_instance;
      ABDE_ref.@A_pure_method(); ~~
         ~[Defs ["ABDE::A_pure_method() [CXXMethod]",
                 "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABDE_ref.@AB_method(0); ~~
         ~[Defs ["ABDE::AB_method(int) [CXXMethod]",
                 "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABDE_ref.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_ref.ABD::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_ref.ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~

      return 0;
    }
  |]
