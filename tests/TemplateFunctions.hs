{-# LANGUAGE QuasiQuotes #-}

module TemplateFunctions (testTemplateFunctions) where

import Pygmalion.Test
import Pygmalion.Test.TH

testTemplateFunctions = runPygmalionTest "template-functions.cpp" $ [pygTest|
    template<int N>
    int foo(const char (&v)[N])
    {
      return N;
    }
    
    int main(int argc, char** argv)
    {
      return @foo("test"); ~[Def "foo(const char (&)[N]) [FunctionTemplate]"]~
    }
  |]
