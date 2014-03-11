{-# LANGUAGE OverloadedStrings #-}

module Hiak.VClock (
   -- * types
     VClock
   , Clock(..)
   -- * functions
   , empty
   , lookup
   , increment
   , increment1
   , descends
   , (===>)
   , merge
   , dominates
   , equal
   , concurrent
) where

import Prelude hiding (lookup)
import qualified Data.List as L
import Data.Monoid (mappend)

type VClockCounter = Integer
type VClockTimestamp = Integer

data Clock a = Clock { actor :: a
                     , counter :: VClockCounter
                     , timestamp :: VClockTimestamp
                     } deriving (Show, Eq)

instance (Ord a) => Ord (Clock a) where
    compare (Clock aa ca ta) (Clock ab cb tb)
      = compare aa ab `mappend` compare ca cb `mappend` compare ta tb

-- | riak_core-style vector clock.
-- A type parameter is an actor type.
data VClock a = VClock [Clock a] deriving (Show, Eq)

-- | Construct an empty vector clock.
empty :: VClock a
empty = VClock []

-- | Lookup a Clock entry for an actor.
lookup :: (Eq a) => VClock a -> a -> Maybe (Clock a)
lookup (VClock vclock) actor = go vclock
  where
    go (ent@(Clock actor' _ _):vc) | actor' == actor = Just ent
    go (_:vc) = go vc
    go [] = Nothing

-- | Replace a Clock entry with a matching actor using a function. Adds an
-- entry a Clock entry if one is not found.
replace :: (Eq a) => [Clock a] -> a -> (Maybe (Clock a) -> Clock a) -> [Clock a]
replace vclock actor f = replace' vclock [] False
  where 
    replace' (x@(Clock actor' _ _):xs) acc found
        | actor' == actor = replace' xs ((f $ Just x):acc) True
        | otherwise       = replace' xs (x:acc) found
    replace' [] acc False = (f Nothing):acc
    replace' [] acc True = acc

-- | Increment a Clock counter for an actor.
increment :: (Eq a) => VClock a -> a -> VClockTimestamp -> VClock a
increment (VClock vclock) act ts = VClock $ replace vclock act inc
  where
    inc (Just Clock{actor=a, counter=c}) = Clock{actor=a, counter=c+1, timestamp=ts}
    inc Nothing = Clock{actor=act, counter=1, timestamp=ts}

-- | Increment a Clock counter for an actor with timestamp equal to -1.
increment1 vclock act = increment vclock act (-1)

-- | Test if the first vclock is a direct descendant of the second (partial
-- ordering property).
descends :: (Eq a) => VClock a -> VClock a -> Bool
descends = (===>)

-- | Same as 'descends'.
(===>) :: (Eq a) => VClock a -> VClock a -> Bool
a@(VClock va) ===> (VClock vb) = va ==> vb
  where
    va ==> [] = True
    va ==> ((Clock actorB counterB _):vbs) = descends' (lookup a actorB) && va ==> vbs
      where
            descends' (Just Clock{counter=counterA}) = counterA >= counterB
            descends' Nothing = False

-- | Merges two vector clocks.
merge :: (Eq a, Ord a) => VClock a -> VClock a -> VClock a
merge (VClock a) (VClock b) = VClock $ L.foldl' folder a b
  where
    folder va clock@(Clock actor _ _) = replace va actor (\mc -> case mc of
                                                                 Nothing -> clock
                                                                 Just clock' -> max clock clock')

-- | Check if va dominates vb.
dominates :: (Eq a) => VClock a -> VClock a -> Bool
dominates va vb = all (== True) [va ===> vb, not $ vb ===> va]

-- | Check if two vector clocks are equal.
equal :: (Ord a, Eq a) => VClock a -> VClock a -> Bool
equal (VClock va) (VClock vb) = L.sort va == L.sort vb

-- | Check if two vector clocks are concurrent (riak-style).
-- See <https://github.com/basho/riak_core/blob/61c2eafba1fa5da4f7fa14ccb767b1eb3a8e706b/src/vclock.erl#L100 riak_core vclock comments>
concurrent :: (Ord a, Eq a) => VClock a -> VClock a -> Bool
concurrent a b = all (== True) [descends a b, descends b a, not $ equal a b]
