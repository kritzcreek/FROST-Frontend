module Render.Types where

import Data.Tuple
---------------------------------------------------------------------
--| Slots

newtype Slot = Slot { room :: String, time :: Number }
instance eqSlot :: Eq Slot where
  (==) (Slot s1) (Slot s2) = s1.room == s2.room && s1.time == s2.time
  (/=) (Slot s1) (Slot s2) = s1.room /= s2.room || s1.time /= s2.time

---------------------------------------------------------------------
--| Topics

newtype Topic = Topic { description :: String, typ :: String }
instance eqTopic :: Eq Topic where
(==) (Topic t1) (Topic t2) = t1.description == t2.description && t1.typ == t2.typ
(/=) (Topic t1) (Topic t2) = t1.description /= t2.description || t1.typ /= t2.typ

---------------------------------------------------------------------
--| Gesamter AppState

type AppState = { timeslots :: [Tuple Slot Topic] }

---------------------------------------------------------------------
--| Dummy Werte

emptyState = {timeslots : []}

mySlot = Slot {room:"Berlin", time:10}
mySlot1 = Slot {room:"Hamburg", time:200}
myTopic1 = Topic {description:"Reactive Design", typ:"Vorstellung"}
myTopic = Topic {description:"Functional Javascript", typ:"Diskussion"}

myState1 = {timeslots:
  [(Tuple mySlot myTopic)
  ,(Tuple  mySlot1 myTopic1)
  ]}
---------------------------------------------------------------------
