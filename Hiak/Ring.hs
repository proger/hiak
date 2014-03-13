module Hiak.Ring where

data Partition a = Partition a
data Ring a = Ring [Partition a]

-- XXX: services?
data VNode a = Partiton a
data Node a = Node [VNode a]

data PrefList a = PrefList [VNode a]
data Command a o p = Command (PrefList a) o p -- key, payload

data Gossip a = Announce (Partition a) | RingState (Ring a)

