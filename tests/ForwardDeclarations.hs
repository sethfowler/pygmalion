module ForwardDeclarations (testForwardDeclarations) where

import Pygmalion.Test

testForwardDeclarations = runPygmalionTestWithFiles $
  [ ("forward-declarations-before.cpp", do
     line "class is_forward_declared_not_defined_before { };"
    ),
    ("forward-declarations.cpp", do
      test "class @is_forward_declared;" [Def "forward-declarations.cpp:5:7: Definition"]
      line ""
      line "class is_backward_declared { };"
      line ""
      line "class is_forward_declared { };"
      line ""
      test "class @is_backward_declared;" [Def "forward-declarations.cpp:3:7: Definition"]
      line ""
      test "class @is_forward_declared_not_defined_before;"
        [Def "forward-declarations-before.cpp:1:7: Definition"]
      test "class @is_forward_declared_not_defined_after;"
        [Def "forward-declarations-after.cpp:1:7: Definition"]
      line ""
      line "int main(int argc, char** argv)"
      line "{"
      test "  @is_backward_declared a;" [Def "forward-declarations.cpp:3:7: Definition"]
      test "  @is_forward_declared b;" [Def "forward-declarations.cpp:5:7: Definition"]
      test "  @is_forward_declared_not_defined_before* c;"
        [Def "forward-declarations-before.cpp:1:7: Definition"]
      test "  @is_forward_declared_not_defined_after* d;"
        [Def "forward-declarations-after.cpp:1:7: Definition"]
      line "  return 0;"
      line "}"
    ),
    ("forward-declarations-after.cpp", do
     line "class is_forward_declared_not_defined_after { };"
    )
  ]
