module Render.Types where

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

---------------------------------------------------------------------


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

newtype Slot = Slot { room :: Room, block :: Block }
instance eqSlot :: Eq Slot where
  (==) (Slot s1) (Slot s2) = s1.room == s2.room && s1.block == s2.block
  (/=) (Slot s1) (Slot s2) = s1.room /= s2.room || s1.block /= s2.block

instance foreignSlot :: IsForeign Slot where
  read val = do
    room <- readProp "room" val :: F Room
    block <- readProp "block" val :: F Block
    return $ Slot {room: room, block: block}

newtype Room = Room { name :: String, capacity :: Number }
instance showRoom :: Show Room where
  show (Room r) = r.name
instance eqRoom :: Eq Room where
  (==) (Room r1) (Room r2) = r1.name == r2.name
  (/=) (Room r1) (Room r2) = r1.name /= r2.name
instance foreignRoom :: IsForeign Room where
  read val = do
    name     <- readProp "name" val     :: F String
    capacity <- readProp "capacity" val :: F Number
    return $ Room {name: name, capacity: capacity}

newtype Block = Block { start :: String }
instance showBlock :: Show Block where
  show (Block b) = b.start
instance eqBlock :: Eq Block where
  (==) (Block b1) (Block b2) = b1.start == b2.start
  (/=) (Block b1) (Block b2) = b1.start /= b2.start
instance foreignBlock :: IsForeign Block where
  read val = do
    start <- readProp "start" val     :: F String
    return $ Block {start: start}

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
                , rooms :: [Room]
                , blocks :: [Block]
                , slots :: [Slot]
                , timeslots :: [Timeslot]
                , selected :: Maybe Topic
                }

type SanitizedTopic = { description :: String, typ :: String }
type SanitizedSlot =  { room :: String, block :: String }
type SanitizedTimeslot = Tuple SanitizedSlot SanitizedTopic

type SanitizedAppState = { topics :: [SanitizedTopic]
                         , rooms :: [Room]
                         , blocks :: [Block]
                         , slots :: [SanitizedSlot]
                         , timeslots :: [SanitizedTimeslot]
                         , selected :: Maybe SanitizedTopic
                         }
---------------------------------------------------------------------
--| Dummy Values

emptyState = {topics: [], rooms:[], blocks:[], slots: [], timeslots: [], selected: Nothing}

myRoom = Room {name: "Berlin", capacity: 100}
myRoom1 = Room {name: "Hamburg", capacity: 80}
myRoom2 = Room {name: "Köln", capacity: 30}

myBlock = Block {start: "8:00am"}
myBlock1 = Block {start: "12:00am"}

mySlot = Slot {room:myRoom, block:myBlock}
mySlot1 = Slot {room:myRoom1, block:myBlock1}

myTopic = Topic {description:"Purescript is great", typ:Workshop}
myTopic1 = Topic {description:"Reactive Design", typ:Presentation}
myTopic2 = Topic {description:"Functional Javascript", typ:Discussion}

myState1 = { topics: [myTopic, myTopic1, myTopic2]
           , slots : [mySlot, mySlot1]
           , rooms : [myRoom, myRoom1, myRoom2]
           , blocks : [myBlock, myBlock1]
           , timeslots: [Tuple mySlot myTopic, Tuple mySlot1 myTopic1]
           , selected: Nothing :: Maybe Topic}
---------------------------------------------------------------------
