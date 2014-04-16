{-# LANGUAGE BangPatterns #-}

module Pygmalion.Test.TH
( pygTest
) where

import Control.Applicative
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

pygTest = QuasiQuoter
  { quoteExp  = quotePygTest
  , quotePat  = const $ fail $ "illegal pygTest QuasiQuote " ++
                               "(allowed as expression only, used as a pattern)"
  , quoteType = const $ fail $ "illegal pygTest QuasiQuote " ++
                               "(allowed as expression only, used as a type)"
  , quoteDec  = const $ fail $ "illegal pygTest QuasiQuote " ++
                               "(allowed as expression only, used as a declaration)"
  }

quotePygTest :: String -> Q Exp
quotePygTest s = DoE <$> mapM (pygLine indent) ls
 where
   ls = dropWhileEnd (onlySpaces . fst)
      . dropWhile (onlySpaces . fst)
      $ pygLines s

   onlySpaces = all (== ' ')

   indent = minimum
          . map (length . takeWhile (== ' '))
          . filter (not . onlySpaces)
          . map fst
          $ ls

pygLines :: String -> [(String, String)]
pygLines = readLines "" "" []
  where
    readLines !l !t !ls []                     = reverse $ rev l t : ls
    readLines !l !t !ls ('\r':cs)              = readLines l t ls cs
    readLines !l !t !ls ('\n':cs)              = readLines "" [] (rev l t : ls) cs
    readLines !l !t !ls ('~':'[':cs)           = readList l ('[':t) ls cs
    readLines !l !t !ls ('~':'~':'\r':'\n':cs) = readLines l t ls cs
    readLines !l !t !ls ('~':'~':'\n':cs)      = readLines l t ls cs
    readLines !l !t !ls (c:cs)                 = readLines (c:l) t ls cs

    readList !l !t !ls []           = error "Unmatched ~[ in pygTest QuasiQuote"
    readList !l !t !ls ('\r':cs)    = readList l t ls cs
    readList !l !t !ls (']':'~':cs) = readLineEnd l (']':t) ls cs
    readList !l !t !ls (c:cs)       = readList l (c:t) ls cs

    readLineEnd !l !t !ls []             = reverse $ rev l t : ls
    readLineEnd !l !t !ls ('\r':'\n':cs) = readLines "" [] (rev l t : ls) cs
    readLineEnd !l !t !ls ('\n':cs)      = readLines "" [] (rev l t : ls) cs
    readLineEnd !l !t !ls (' ':cs)       = readLineEnd l t ls cs
    readLineEnd !l !t !ls _              = error "]~ must end line in pygTest QuasiQuote"

    rev !l !t = (reverse l, reverse t)

pygLine :: Int -> (String, String) -> Q Stmt
pygLine indent (line, test) = do
    let lineExp = LitE $ StringL $ drop indent line

    testExp <- case (test, parseExp test) of
                 ("", _)        -> return $ ListE []
                 (_, Left err)  -> fail err
                 (_, Right exp) -> return exp

    Just testFuncName <- lookupValueName "test"
    let testFunc = VarE testFuncName

    return $ NoBindS $ AppE (AppE testFunc lineExp) testExp
