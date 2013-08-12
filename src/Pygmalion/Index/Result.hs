module Pygmalion.Index.Result
( LookupInfo(..)
) where

import Pygmalion.Core

data LookupInfo = GotDef DefInfo
                | GotDecl USR DefInfo
                | GotUSR USR
                | GotNothing
                deriving (Eq, Show)
