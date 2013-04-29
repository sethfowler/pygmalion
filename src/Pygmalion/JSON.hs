{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.JSON
( sourceRecordsToJSON
) where

import qualified Data.Text as T
import Text.JSON

import Pygmalion.Core

sourceRecordsToJSON :: [CommandInfo] -> String
sourceRecordsToJSON cis = encodeStrict $ map (toJSObject . toKeyValue) cis
  where toKeyValue (CommandInfo sf wd (Command cmd args) _ _) =
          [("file", unSourceFileText sf),
           ("directory", wd),
           ("command", T.intercalate " " (cmd : args))]
