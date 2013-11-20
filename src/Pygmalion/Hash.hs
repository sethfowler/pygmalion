module Pygmalion.Hash
( hash
, hashInt
, combineHash
, combineHashInt
) where

import qualified Data.ByteString.UTF8 as B
import Data.Digest.CityHash
import Data.Hashable (hashWithSalt)
import Data.Int

hash :: B.ByteString -> Int64
hash = fromIntegral . flip cityHash64WithSeed 14695981039346656037
{-# INLINE hash #-}

hashInt :: B.ByteString -> Int
hashInt = fromIntegral . flip cityHash64WithSeed 14695981039346656037
{-# INLINE hashInt #-}

combineHash :: Int64 -> Int64 -> Int64
combineHash a b = fromIntegral $ hashWithSalt (toInt a) (toInt b)
  where
    toInt :: Int64 -> Int
    toInt = fromIntegral
{-# INLINE combineHash #-}

combineHashInt :: Int64 -> Int -> Int64
combineHashInt a b = fromIntegral $ hashWithSalt (toInt a) b
  where
    toInt :: Int64 -> Int
    toInt = fromIntegral
{-# INLINE combineHashInt #-}
