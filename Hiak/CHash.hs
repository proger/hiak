-- |
-- Consistent hashing implementation
-- <http://thor.cs.ucsb.edu/~ravenben/papers/coreos/kll+97.pdf>

{-# LANGUAGE BangPatterns, ScopedTypeVariables, OverloadedStrings #-}

module Hiak.CHash where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.List as L
-- import qualified Data.Hashable as H

data Owner a = Owner a deriving (Show)
data Ring a = Ring {
              ringPartitions :: Integer
            , ringSpace :: [(Owner a, Integer)]
            } deriving (Show)

singleton :: forall a. Integer -> Owner a -> Ring a
singleton partitions node = Ring partitions space
  where
    space :: [(Owner a, Integer)]
    space = zip (repeat node) [0,(step partitions)..maxring]

maxring = 2^160-1 -- SHA1 key space
step = (maxring `div`)

r5 = singleton 5 $ Owner "yo"

index :: LBS.ByteString -> Integer
index = SHA.integerDigest . SHA.sha1

successors ring@Ring{ringPartitions=parts, ringSpace=space} i = take lim $ drop off $ cycle space
  where
    off = fromInteger $ i `div` (step parts)
    lim = fromInteger parts

preflist = successors

insert ring node = undefined
