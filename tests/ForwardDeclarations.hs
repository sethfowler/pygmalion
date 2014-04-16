{-# LANGUAGE QuasiQuotes #-}

module ForwardDeclarations (testForwardDeclarations) where

import Pygmalion.Test
import Pygmalion.Test.TH

testForwardDeclarations = runPygmalionTestWithFiles $
  [ ("forward-declarations-before.cpp", [pygTest|
     class is_forward_declared_not_defined_before { };
    |]),
    ("forward-declarations.cpp", [pygTest|
      class @is_forward_declared; ~[Def "forward-declarations.cpp:5:7: Definition"]~
      
      class is_backward_declared { };
      
      class is_forward_declared { };
      
      class @is_backward_declared; ~[Def "forward-declarations.cpp:3:7: Definition"]~
      
      class @is_forward_declared_not_defined_before; ~~
        ~[Def "forward-declarations-before.cpp:1:7: Definition"]~
      class @is_forward_declared_not_defined_after; ~~
        ~[Def "forward-declarations-after.cpp:1:7: Definition"]~
      
      int main(int argc, char** argv)
      {
        @is_backward_declared a; ~[Def "forward-declarations.cpp:3:7: Definition"]~
        @is_forward_declared b; ~[Def "forward-declarations.cpp:5:7: Definition"]~
        @is_forward_declared_not_defined_before* c; ~~
          ~[Def "forward-declarations-before.cpp:1:7: Definition"]~
        @is_forward_declared_not_defined_after* d; ~~
          ~[Def "forward-declarations-after.cpp:1:7: Definition"]~
        return 0;
      }
    |]),
    ("forward-declarations-after.cpp", [pygTest|
     class is_forward_declared_not_defined_after { };
    |])
  ]
