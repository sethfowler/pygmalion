module TemplateFunctions (testTemplateFunctions) where

import Pygmalion.Test

testTemplateFunctions = runPygmalionTest "template-functions.cpp" $ do
  line "template<int N>"
  line "int foo(const char (&v)[N])"
  line "{"
  line "  return N;"
  line "}"
  line ""
  line "int main(int argc, char** argv)"
  line "{"
  test "  return @foo(\"test\");" [Def "foo(const char (&)[N]) [FunctionTemplate]"]
  line "}"
