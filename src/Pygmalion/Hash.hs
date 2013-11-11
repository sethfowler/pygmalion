module Pygmalion.Hash
( hash
, hashInt
) where

import qualified Data.ByteString.UTF8 as B
import Data.Digest.CityHash
import Data.Int

hash :: B.ByteString -> Int64
hash = fromIntegral . (flip cityHash64WithSeed) 14695981039346656037

hashInt :: B.ByteString -> Int
hashInt = fromIntegral . (flip cityHash64WithSeed) 14695981039346656037
