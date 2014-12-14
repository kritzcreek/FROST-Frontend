module Render.Types where

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

--import Data.JSON
---------------------------------------------------------------------
newtype Room = Room { name :: String, capacity :: Number }

data TopicType = Discussion
               | Presentation
               | Workshop

instance eqTopicType :: Eq TopicType where
(==) tt1 tt2 = show tt1 == show tt2
(/=) tt1 tt2 = show tt1 /= show tt2



instance showTopicType :: Show TopicType where
  show Discussion = "Discussion"
  show Presentation = "Presentation"
  show Workshop = "Workshop"

instance foreignTopicType :: IsForeign TopicType where
    read val = case readString val of
      Right "Discussion"   -> Right Discussion
      Right "Presentation" -> Right Presentation
      Right "Workshop"     -> Right Workshop
      _                    -> Left $ JSONError "Cant read TopicType"


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

---------------------------------------------------------------------
--| Topics

newtype Topic = Topic { description :: String, typ :: TopicType }

instance eqTopic :: Eq Topic where
(==) (Topic t1) (Topic t2) = t1.description == t2.description && t1.typ == t2.typ
(/=) (Topic t1) (Topic t2) = t1.description /= t2.description || t1.typ /= t2.typ

instance foreignTopic :: IsForeign Topic where
  read val = do
    description <- readProp "description" val :: F String
    typ         <- readProp "typ"         val :: F TopicType
    return $ Topic {description: description, typ: typ}
---------------------------------------------------------------------
--| Gesamter AppState

type Timeslot = Tuple Slot Topic

type AppState = { topics :: [Topic]
                , slots :: [Slot]
                , timeslots :: [Timeslot]
                , selected :: Maybe Topic
                }

type SanitizedTopic = { description :: String, typ :: String }

type SanitizedAppState = { topics :: [SanitizedTopic]
                         , slots :: [Slot]
                         , timeslots :: [Timeslot]
                         , selected :: Maybe SanitizedTopic
                         }
---------------------------------------------------------------------
--| Dummy Values

emptyState = {topics: [], slots: [], timeslots: [], selected: Nothing}

mySlot = Slot {room:"Berlin", time:10}
mySlot1 = Slot {room:"Hamburg", time:200}
myTopic2 = Topic {description:"Purescript is great", typ:Workshop}
myTopic1 = Topic {description:"Reactive Design", typ:Presentation}
myTopic = Topic {description:"Functional Javascript", typ:Discussion}

myState1 = { topics: [myTopic, myTopic1, myTopic2]
           , slots : [mySlot, mySlot1]
           , timeslots: [Tuple mySlot myTopic, Tuple mySlot1 myTopic1]
           , selected: Nothing :: Maybe Topic}
---------------------------------------------------------------------
