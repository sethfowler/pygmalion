{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Pygmalion.JSON
( sourceRecordsToJSON
) where

import qualified Data.ByteString.UTF8 as B
import Data.List
import Text.JSON

import Pygmalion.Core

sourceRecordsToJSON :: [CommandInfo] -> String
sourceRecordsToJSON cis = encodeStrict $ map (toJSObject . toKeyValue) cis
  where toKeyValue CommandInfo {..} =
          [("file", unSourceFile ciSourceFile),
           ("directory", B.toString ciWorkingPath),
           ("command", intercalate " " $ map B.toString (ciCommand : ciArgs))]
