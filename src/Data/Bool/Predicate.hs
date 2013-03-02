module Data.Bool.Predicate
( (.&&.)
) where

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g v = f v && g v
