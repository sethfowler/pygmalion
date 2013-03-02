import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Function

import Data.Bool.Predicate

main:: IO ()
main = defaultMain tests

tests =
  [
    testGroup "Data.Bool.Predicate"
    [
      testProperty "Predicate Composition" prop_predicate_composition
    ]
  ]

prop_predicate_composition :: (Fun Int Bool) -> (Fun Int Bool) -> Int -> Bool
prop_predicate_composition (Fun _ f) (Fun _ g) v = ((f .&&. g) v) == ((f v) && (g v))
