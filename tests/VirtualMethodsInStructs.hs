{-# LANGUAGE QuasiQuotes #-}

module VirtualMethodsInStructs (testVirtualMethodsInStructs) where

import Pygmalion.Test
import Pygmalion.Test.TH

testVirtualMethodsInStructs = runPygmalionTest "virtual-in-structs.cpp" $ [pygTest|
    #include "virtual.h"
    
    struct ABDE_struct { ABDE var; };
    struct ABDE_ptr_struct { ABDE* ptr; };
    struct ABDE_ref_struct { ABDE& ref; };
    
    int main(int argc, char** argv)
    {
      ABDE ABDE_instance;
      ABDE* ABDE_ptr = &ABDE_instance;
    
      // Structs containing instance values.
      ABDE_struct ABDE_struct_instance;
      ABDE_struct_instance.var.@A_pure_method(); ~[Def "ABDE::A_pure_method() [CXXMethod]"]~
      ABDE_struct_instance.var.@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
      ABDE_struct_instance.var.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_struct_instance.var.ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
    
      ABDE_struct* ABDE_struct_ptr = &ABDE_struct_instance;
      ABDE_struct_ptr->var.@A_pure_method(); ~[Def "ABDE::A_pure_method() [CXXMethod]"]~
      ABDE_struct_ptr->var.@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
      ABDE_struct_ptr->var.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_struct_ptr->var.ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
    
      ABDE_struct& ABDE_struct_ref = ABDE_struct_instance;
      ABDE_struct_ref.var.@A_pure_method(); ~[Def "ABDE::A_pure_method() [CXXMethod]"]~
      ABDE_struct_ref.var.@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
      ABDE_struct_ref.var.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_struct_ref.var.ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
    
      // Structs containing pointers to instances.
      ABDE_ptr_struct ABDE_ptr_struct_instance { ABDE_ptr };
      ABDE_ptr_struct_instance.ptr->@A_pure_method(); ~~
        ~[Defs ["ABDE::A_pure_method() [CXXMethod]",
                "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABDE_ptr_struct_instance.ptr->@AB_method(0); ~~
        ~[Defs ["ABDE::AB_method(int) [CXXMethod]",
                "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABDE_ptr_struct_instance.ptr->AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_ptr_struct_instance.ptr->ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
    
      ABDE_ptr_struct* ABDE_ptr_struct_ptr = &ABDE_ptr_struct_instance;
      ABDE_ptr_struct_ptr->ptr->@A_pure_method(); ~~
        ~[Defs ["ABDE::A_pure_method() [CXXMethod]",
                "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABDE_ptr_struct_ptr->ptr->@AB_method(0); ~~
        ~[Defs ["ABDE::AB_method(int) [CXXMethod]",
                "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABDE_ptr_struct_ptr->ptr->AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_ptr_struct_ptr->ptr->ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
    
      ABDE_ptr_struct& ABDE_ptr_struct_ref = ABDE_ptr_struct_instance;
      ABDE_ptr_struct_ref.ptr->@A_pure_method(); ~~
        ~[Defs ["ABDE::A_pure_method() [CXXMethod]",
                "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABDE_ptr_struct_ref.ptr->@AB_method(0); ~~
        ~[Defs ["ABDE::AB_method(int) [CXXMethod]",
                "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABDE_ptr_struct_ref.ptr->AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_ptr_struct_ref.ptr->ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
    
      // Structs containing references to instances.
      ABDE_ref_struct ABDE_ref_struct_instance { ABDE_instance };
      ABDE_ref_struct_instance.ref.@A_pure_method(); ~~
        ~[Defs ["ABDE::A_pure_method() [CXXMethod]",
                "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABDE_ref_struct_instance.ref.@AB_method(0); ~~
        ~[Defs ["ABDE::AB_method(int) [CXXMethod]",
                "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABDE_ref_struct_instance.ref.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_ref_struct_instance.ref.ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
    
      ABDE_ref_struct* ABDE_ref_struct_ptr = &ABDE_ref_struct_instance;
      ABDE_ref_struct_ptr->ref.@A_pure_method(); ~~
        ~[Defs ["ABDE::A_pure_method() [CXXMethod]",
                "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABDE_ref_struct_ptr->ref.@AB_method(0); ~~
        ~[Defs ["ABDE::AB_method(int) [CXXMethod]",
                "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABDE_ref_struct_ptr->ref.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_ref_struct_ptr->ref.ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
    
      ABDE_ref_struct& ABDE_ref_struct_ref = ABDE_ref_struct_instance;
      ABDE_ref_struct_ref.ref.@A_pure_method(); ~~
        ~[Defs ["ABDE::A_pure_method() [CXXMethod]",
                "ABDEF::A_pure_method() [CXXMethod]"]]~
      ABDE_ref_struct_ref.ref.@AB_method(0); ~~
        ~[Defs ["ABDE::AB_method(int) [CXXMethod]",
                "ABDEF::AB_method(int) [CXXMethod]"]]~
      ABDE_ref_struct_ref.ref.AB::@AB_method(0); ~[Def "AB::AB_method(int) [CXXMethod]"]~
      ABDE_ref_struct_ref.ref.ABDE::@AB_method(0); ~[Def "ABDE::AB_method(int) [CXXMethod]"]~
    
      return 0;
    }
  |]
