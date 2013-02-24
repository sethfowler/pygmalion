module Pygmalion.JSON
( sourceRecordsToJSON
) where

import Database.SQLite
import Text.JSON

instance JSON Value where
  showJSON (Double d) = showJSON d
  showJSON (Int i)    = showJSON i
  showJSON (Text t)   = showJSON t
  showJSON (Blob b)   = showJSON b
  showJSON (Null)     = JSNull

  -- Don't need to read.
  readJSON _ = Error "Not implemented"

sourceRecordsToJSON rs = encodeStrict $ map toJSObject rs
