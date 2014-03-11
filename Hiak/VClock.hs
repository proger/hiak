{-# LANGUAGE OverloadedStrings #-}

module Hiak.VClock where

-- import Data.Text as T
-- import Data.Time.Clock.POSIX (POSIXTime(..), getPOSIXTime)
import Data.List as L

type VClockNode = String
type VClockCounter = Integer
type VClockTimestamp = Integer

data VClockE = VClockE {
                   eNode :: VClockNode
                 , eCounter :: VClockCounter
                 , eTimestamp :: VClockTimestamp
                 } deriving (Show, Eq)

instance Ord VClockE where
    compare (VClockE node1 _ _) (VClockE node2 _ _) = compare node1 node2

type VClock = [VClockE]

empty :: VClock
empty = []

fromEntry :: VClockNode -> VClockCounter -> VClockTimestamp -> VClock
fromEntry node counter ts = 
    [VClockE{eNode=node, eCounter=counter, eTimestamp=ts}]

findEntry :: VClock -> VClockNode -> Maybe VClockE
findEntry (entry@(VClockE node' _ _):vc) node | node' == node = Just entry
findEntry (_:vc) node = findEntry vc node
findEntry [] node = Nothing

descends :: VClock -> VClock -> Bool
descends _ [] = True
descends va (VClockE{eNode=nodeB, eCounter=ctrB, eTimestamp=tsB}:vbs) = 
    lastCounterGTE lastEntry && descends va (vbs)
      where
        lastEntry = findEntry va nodeB

        lastCounterGTE (Just VClockE{eCounter=ctrA}) = ctrA >= ctrB
        lastCounterGTE Nothing = False

increment :: VClock -> VClockNode -> VClockTimestamp -> VClock
increment vclock node ts = inc' vclock []
  where 
    inc' ((VClockE node' c _):xs) acc | node' == node = (VClockE{eNode=node, eCounter=c+1, eTimestamp=ts}):acc
    inc' (x:xs) acc = inc' xs (x:acc)
    inc' [] acc = (VClockE{eNode=node, eCounter=1, eTimestamp=ts}):acc


dominates :: VClock -> VClock -> Bool
dominates va vb = all (== True) [descends va vb, not $ descends vb va]

merge :: [VClock] -> VClock
merge = undefined
