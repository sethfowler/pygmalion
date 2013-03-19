module Pygmalion.JSON
( sourceRecordsToJSON
) where

import Data.List
import Text.JSON

import Pygmalion.Core

sourceRecordsToJSON :: [CommandInfo] -> String
sourceRecordsToJSON cis = encodeStrict $ map (toJSObject . toKeyValue) cis
  where toKeyValue (CommandInfo sf wd (Command cmd args) _) =
          [("file", sf), ("directory", wd), ("command", intercalate " " (cmd : args))]
