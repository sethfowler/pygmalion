{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.JSON
( sourceRecordsToJSON
) where

import qualified Data.ByteString.UTF8 as B
import Data.List
import Text.JSON

import Pygmalion.Core

sourceRecordsToJSON :: [CommandInfo] -> String
sourceRecordsToJSON cis = encodeStrict $ map (toJSObject . toKeyValue) cis
  where toKeyValue (CommandInfo sf wd cmd args _ _) =
          [("file", unSourceFile sf),
           ("directory", B.toString wd),
           ("command", intercalate " " $ map B.toString (cmd : args))]
