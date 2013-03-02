module Pygmalion.JSON
( sourceRecordsToJSON
) where

import Database.SQLite
import Text.JSON

newtype DBJSONVal = DBJSONVal Value

instance JSON DBJSONVal where
  showJSON (DBJSONVal (Double d)) = showJSON d
  showJSON (DBJSONVal (Int i))    = showJSON i
  showJSON (DBJSONVal (Text t))   = showJSON t
  showJSON (DBJSONVal (Blob b))   = showJSON b
  showJSON (DBJSONVal (Null))     = JSNull

  -- Don't need to read.
  readJSON _ = Error "Not implemented"

sourceRecordsToJSON :: [[(String, Value)]] -> String
sourceRecordsToJSON rs = encodeStrict $ map (toJSObject . wrap) rs
  where wrap = map $ \(s,v) -> (s, DBJSONVal v)
