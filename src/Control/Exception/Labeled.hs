module Control.Exception.Labeled
( labeledCatch
) where

import Prelude hiding (catch)
import Control.Exception (catch, SomeException)

-- Labels and rethrows an exception.
labeledCatch :: String -> IO a -> IO a
labeledCatch lbl f = catch f labelAndRethrow
  where labelAndRethrow :: SomeException -> IO a
        labelAndRethrow e = error $ lbl ++ ": " ++ (show e)
