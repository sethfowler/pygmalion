module Macros (testMacros) where

import Pygmalion.Test

testMacros = runPygmalionTest "macros.cpp" $ do
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
