module Openspace.Types where

import           Data.Either
import           Data.Foreign
import Data.List (toList)
import           Data.Foreign.Class
import           Data.Foreign.Index
import qualified Data.Map as M
import           Data.Maybe
import           Data.Tuple
import           Prelude

-----------------
--| TopicType |--
-----------------

data TopicType = Discussion
               | Presentation
               | Workshop

instance eqTopicType :: Eq TopicType where
  eq tt1 tt2 = show tt1 == show tt2

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

-- Used to fill Dropdowns etc.
topicTypes = [Discussion, Presentation, Workshop]

--------------
--| Topics |--
--------------

newtype Topic = Topic { description :: String
                      , typ   :: TopicType }

instance eqTopic :: Eq Topic where
  eq (Topic t1) (Topic t2) = t1.description == t2.description && t1.typ == t2.typ

instance foreignTopic :: IsForeign Topic where
  read val = Topic <$> (
    { description: _, typ: _ } <$>
    readProp "description" val <*>
    readProp "typ" val 
    )

------------
--| Slot |--
------------

newtype Slot = Slot { room :: Room, block :: Block }

instance eqSlot :: Eq Slot where
  eq (Slot s1) (Slot s2) = s1.room == s2.room && s1.block == s2.block

instance ordSlot :: Ord Slot where
  compare (Slot s1) (Slot s2) =
    (show s1.room ++ show s1.block) `compare` (show s2.room ++ show s2.block)

instance showSlot :: Show Slot where
  show (Slot s) = "{ room: " ++ show s.room ++" block: " ++ show s.block ++ "}"

instance foreignSlot :: IsForeign Slot where
  read val = Slot <$> (
    { room: _, block: _} <$>
    readProp "room" val  <*>
    readProp "block" val
    )

------------
--| Room |--
------------

newtype Room = Room { name :: String, capacity :: Number }

instance showRoom :: Show Room where
  show (Room r) = r.name

instance eqRoom :: Eq Room where
  eq (Room r1) (Room r2) = r1.name == r2.name

instance foreignRoom :: IsForeign Room where
  read val = Room <$> (
    { name: _, capacity: _} <$>
    readProp "name" val     <*>
    readProp "capacity" val
    )

-------------
--| Block |--
-------------

newtype Block = Block { description :: String
                      , startHours :: Number
                      , startMinutes :: Number
                      , endHours :: Number
                      , endMinutes :: Number}

instance showBlock :: Show Block where
  show (Block b) = b.description

instance eqBlock :: Eq Block where
  eq (Block b1) (Block b2) = b1.description == b2.description
                               && b1.startHours == b2.startHours
                               && b1.startMinutes == b2.startMinutes
                               && b1.endHours == b2.endHours
                               && b1.endMinutes == b2.endMinutes

instance ordBlock :: Ord Block where
  compare (Block b1) (Block b2) = compare (hourDiff * 60.0 + minDiff) 0.0
    where hourDiff = b1.startHours - b2.startHours
          minDiff = b1.startMinutes - b2.startMinutes



instance foreignBlock :: IsForeign Block where
  read val = Block <$> (
    { description: _
    , startHours: _
    , startMinutes: _
    , endHours: _
    , endMinutes: _
    } <$>
    readProp "description" val <*>
    readProp "startHours" val <*>
    readProp "startMinutes" val <*>
    readProp "endHours" val <*>
    readProp "endMinutes" val
    )

---------------
--| Actions |--
---------------

data Action = AddTopic Topic
            | DeleteTopic Topic
            | AddRoom Room
            | DeleteRoom Room
            | AddBlock Block
            | DeleteBlock Block
            | AssignTopic Slot Topic
            | UnassignTopic Topic
            | ReplayActions (Array Action)
            | ShowError String
            | NOP

instance foreignAction :: IsForeign Action where
read val = case readProp "tag" val of
  Right "AddTopic" -> AddTopic <$> readProp "contents" val
  Right "DeleteTopic" -> DeleteTopic <$> readProp "contents" val
  Right "AddRoom" -> AddRoom <$> readProp "contents" val
  Right "DeleteRoom" -> DeleteRoom <$> readProp "contents" val
  Right "AddBlock" -> AddBlock <$> readProp "contents" val
  Right "DeleteBlock" -> DeleteBlock <$> readProp "contents" val
  Right "AssignTopic" -> let val' = parseAssignTopic val
                         in AssignTopic <$> readProp "slot" val' <*> readProp "topic" val'
  Right "UnassignTopic" -> UnassignTopic <$> readProp "contents" val
  Right "ReplayEvents" -> ReplayActions <$> readProp "contents" val
  Right "ShowError" -> ShowError <$> readProp "message" val
  Left e -> Right $ ShowError (show e)

class AsForeign a where
  serialize :: a -> Foreign

instance actionAsForeign :: AsForeign Action where
  serialize (AddTopic (Topic t)) =
    toForeign { tag: "AddTopic"
              , contents: { description: t.description
                          , typ: show t.typ
                          }
              }

  serialize (DeleteTopic (Topic t)) =
    toForeign { tag: "DeleteTopic"
              , contents: { description: t.description
                          , typ: show t.typ
                          }
              }
  serialize (AddRoom (Room r)) =
    toForeign { tag: "AddRoom"
              , contents: r }

  serialize (DeleteRoom (Room r)) =
    toForeign { tag: "DeleteRoom"
              , contents: r }

  serialize (AddBlock (Block b)) =
    toForeign { tag: "AddBlock"
              , contents: b }

  serialize (DeleteBlock (Block b)) =
    toForeign { tag: "DeleteBlock"
              , contents: b }

  serialize (AssignTopic s t) = serializeAssignTopic s t
  serialize (UnassignTopic (Topic t)) =
    toForeign { tag: "UnassignTopic"
              , contents: { description: t.description
                          , typ: show t.typ
                          }
              }

foreign import parseAssignTopic :: Foreign -> Foreign


foreign import serializeAssignTopic :: Slot -> Topic -> Foreign

-------------------------
--| Entire AppState |--
-------------------------

type Timeslot = Tuple Slot Topic

type AppState = { topics :: Array Topic
                , rooms :: Array Room
                , blocks :: Array Block
                , timeslots :: M.Map Slot Topic
                }

 --------------------
 --| Dummy Values |--
 --------------------

emptyState = {topics: [], rooms:[], blocks:[], timeslots: (M.empty) :: M.Map Slot Topic }

myRoom = Room {name: "Berlin", capacity: 100.0}
myRoom1 = Room {name: "Hamburg", capacity: 80.0}
myRoom2 = Room {name: "Koeln", capacity: 30.0}

myBlock = Block {
  description:"First",
  startHours: 8.0,
  startMinutes: 0.0,
  endHours: 10.0,
  endMinutes: 0.0}
myBlock1 = Block {
  description:"Second",
  startHours: 8.0,
  startMinutes: 0.0,
  endHours: 10.0,
  endMinutes: 0.0}

mySlot = Slot {room:myRoom, block:myBlock}
mySlot1 = Slot {room:myRoom1, block:myBlock1}

myTopic = Topic  {description:"Purescript is great", typ:Workshop}
myTopic1 = Topic {description:"Reactive Design", typ:Presentation}
myTopic2 = Topic {description:"Functional Javascript", typ:Discussion}
myTopic3 = Topic {description:"Enemy of the State", typ:Presentation}
myTopic4 = Topic {description:"Wayyyyyyy too long name for a Topic.", typ:Workshop}
myTopic5 = Topic {description:"fix", typ:Discussion}

myState1 = { topics: [myTopic, myTopic1, myTopic2, myTopic3, myTopic4, myTopic5]
           , rooms : [myRoom, myRoom1, myRoom2]
           , blocks : [myBlock, myBlock1]
           , timeslots: M.fromList $ toList [Tuple mySlot myTopic, Tuple mySlot1 myTopic1]
           }
