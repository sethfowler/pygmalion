{-# LANGUAGE QuasiQuotes #-}

module Macros (testMacros) where

import Pygmalion.Test
import Pygmalion.Test.TH

testMacros = runPygmalionTest "macros.cpp" $ [pygTest|
    #define VAR 0
    #define VARF(x) (int) x

    int main(int argc, char** argv)
    {
      char local_var = 0;

      return @VAR                ~[Def "VAR [MacroDefinition]"]~
           + @VARF(@local_var);  ~[Def "VARF [MacroDefinition]", Fails "#105"]~
    }
  |]
