module Render.Types where

import Data.Tuple
import Data.Maybe
import Data.Foreign
import Data.Foreign.Class

--import Data.JSON
---------------------------------------------------------------------
--| Slots

newtype Slot = Slot { room :: String, time :: Number }
instance eqSlot :: Eq Slot where
  (==) (Slot s1) (Slot s2) = s1.room == s2.room && s1.time == s2.time
  (/=) (Slot s1) (Slot s2) = s1.room /= s2.room || s1.time /= s2.time

instance foreignSlot :: IsForeign Slot where
  read val = do
    room <- readProp "room" val :: F String
    time <- readProp "time" val :: F Number
    return $ Slot {room: room, time: time}

{-
instance fromJSONSlot :: FromJSON Slot where
  parseJSON (JObject o) = do
    room <- o .: "room"
    time <- o .: "time"
    return $ Slot {room:room, time:time}
  parseJSON _ = fail "Slot parse failed."

instance toJSONSlot :: ToJSON Slot where
  toJSON (Topic t) = object ["room" .= t.room, "time" .= t.time]
-}

---------------------------------------------------------------------
--| Topics

newtype Topic = Topic { description :: String, typ :: String }

instance eqTopic :: Eq Topic where
(==) (Topic t1) (Topic t2) = t1.description == t2.description && t1.typ == t2.typ
(/=) (Topic t1) (Topic t2) = t1.description /= t2.description || t1.typ /= t2.typ

instance foreignTopic :: IsForeign Topic where
  read val = do
    description <- readProp "description" val :: F String
    typ         <- readProp "typ"         val :: F String
    return $ Topic {description: description, typ: typ}
---------------------------------------------------------------------
--| Gesamter AppState

type Timeslot = Tuple Slot Topic

type AppState = { timeslots :: [Timeslot], selected :: Maybe Topic }

---------------------------------------------------------------------
--| Dummy Werte

emptyState = {timeslots : [], selected: Nothing}

mySlot = Slot {room:"Berlin", time:10}
mySlot1 = Slot {room:"Hamburg", time:200}
myTopic1 = Topic {description:"Reactive Design", typ:"Vorstellung"}
myTopic = Topic {description:"Functional Javascript", typ:"Diskussion"}

myState1 = {timeslots:
  [(Tuple mySlot myTopic)
  ,(Tuple  mySlot1 myTopic1)
  ], selected: Nothing :: Maybe Topic}
---------------------------------------------------------------------
