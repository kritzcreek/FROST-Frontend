module Openspace.Types where

import           Data.Either
import           Data.Foreign
import           Data.Foreign.Class
import           Data.Foreign.Index
import qualified Data.Map as M
import           Data.Maybe
import           Data.Tuple

-----------------
--| TopicType |--
-----------------

data TopicType = Discussion
               | Presentation
               | Workshop

instance eqTopicType :: Eq TopicType where
(==) tt1 tt2 = show tt1 == show tt2
(/=) tt1 tt2 = show tt1 /= show tt2

instance showTopicType :: Show TopicType where
  show Discussion = "Diskussion"
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

newtype Topic = Topic { topicDescription :: String
                      , topicTyp   :: TopicType }

instance eqTopic :: Eq Topic where
  (==) (Topic t1) (Topic t2) = t1.topicDescription == t2.topicDescription && t1.topicTyp == t2.topicTyp
  (/=) t1 t2 = not (t1 == t2)

instance foreignTopic :: IsForeign Topic where
    read val = do
      topic <- readProp "topicDescription" val :: F String
      typ   <- readProp "topicTyp"   val :: F TopicType
      return $ Topic { topicDescription: topic, topicTyp: typ }


------------
--| Slot |--
------------

newtype Slot = Slot { room :: Room, block :: Block }

instance eqSlot :: Eq Slot where
  (==) (Slot s1) (Slot s2) = s1.room == s2.room && s1.block == s2.block
  (/=) (Slot s1) (Slot s2) = s1.room /= s2.room || s1.block /= s2.block

instance ordSlot :: Ord Slot where
  compare (Slot s1) (Slot s2) = (show s1.room ++ show s1.block) `compare` (show s2.room ++ show s2.block)

instance showSlot :: Show Slot where
  show (Slot s) = "{ room: " ++ show s.room ++" block: " ++ show s.block ++ "}"

instance foreignSlot :: IsForeign Slot where
  read val = do
    room <- readProp "room" val :: F Room
    block <- readProp "block" val :: F Block
    return $ Slot {room: room, block: block}

------------
--| Room |--
------------

newtype Room = Room { roomName :: String, roomCapacity :: Number }

instance showRoom :: Show Room where
  show (Room r) = r.roomName

instance eqRoom :: Eq Room where
  (==) (Room r1) (Room r2) = r1.roomName == r2.roomName
  (/=) (Room r1) (Room r2) = r1.roomName /= r2.roomName

instance foreignRoom :: IsForeign Room where
  read val = do
    name     <- readProp "roomName" val     :: F String
    capacity <- readProp "roomCapacity" val :: F Number
    return $ Room {roomName: name, roomCapacity: capacity}

-------------
--| Block |--
-------------

newtype Block = Block { blockDescription :: String
                      , blockStartHours :: Number
                      , blockStartMinutes :: Number
                      , blockEndHours :: Number
                      , blockEndMinutes :: Number}

instance showBlock :: Show Block where
  show (Block b) = b.blockDescription

instance eqBlock :: Eq Block where
  (==) (Block b1) (Block b2) = b1.blockDescription == b2.blockDescription
                               && b1.blockStartHours == b2.blockStartHours
                               && b1.blockStartMinutes == b2.blockStartMinutes
                               && b1.blockEndHours == b2.blockEndHours
                               && b1.blockEndMinutes == b2.blockEndMinutes

  (/=) b1 b2 = not (b1 == b2)

instance ordBlock :: Ord Block where
  compare (Block b1) (Block b2) = compare (hourDiff * 60 + minDiff) 0
    where hourDiff = b1.blockStartHours - b2.blockStartHours
          minDiff = b1.blockStartMinutes - b2.blockStartMinutes



instance foreignBlock :: IsForeign Block where
  read val = do
    description <- readProp "blockDescription" val
    startHours <- readProp "blockStartHours" val
    startMinutes <- readProp "blockStartMinutes" val
    endHours <- readProp "blockEndHours" val
    endMinutes <- readProp "blockEndMinutes" val
    return $ Block { blockDescription: description
                   , blockStartHours: startHours
                   , blockStartMinutes: startMinutes
                   , blockEndHours: endHours
                   , blockEndMinutes: endMinutes
                   }

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
            | ReplayActions [Action]
            | ShowError String
            | NOP

instance foreignAction :: IsForeign Action where
read val = case readProp "tag" val of
  Right "AddTopic" -> do
    t <- readProp "contents" val :: F Topic
    return $ AddTopic t
  Right "DeleteTopic" -> do
    t <- readProp "contents" val :: F Topic
    return $ DeleteTopic t
  Right "AddRoom" -> do
    r <- readProp "contents" val :: F Room
    return $ AddRoom r
  Right "DeleteRoom" -> do
    r <- readProp "contents" val :: F Room
    return $ DeleteRoom r
  Right "AddBlock" -> do
    b <- readProp "contents" val :: F Block
    return $ AddBlock b
  Right "DeleteBlock" -> do
    b <- readProp "contents" val :: F Block
    return $ DeleteBlock b
  Right "AssignTopic" -> do
    let val' = parseAssignTopic val
    s <- readProp "slot" val' :: F Slot
    t <- readProp "topic" val' :: F Topic
    return $ AssignTopic s t
  Right "UnassignTopic" -> do
    t <- readProp "contents" val :: F Topic
    return $ UnassignTopic t
  Right "ReplayEvents" -> do
    actions <- readProp "contents" val :: F [Action]
    return $ ReplayActions actions
  Right "ShowError" -> do
    m <- readProp "message" val :: F String
    return $ ShowError m
  Left e -> Right $ ShowError (show e)

class AsForeign a where
  serialize :: a -> Foreign

instance actionAsForeign :: AsForeign Action where
  serialize (AddTopic (Topic t)) =
    toForeign { tag: "AddTopic"
              , contents: { topicDescription: t.topicDescription
                          , topicTyp: show t.topicTyp
                          }
              }

  serialize (DeleteTopic (Topic t)) =
    toForeign { tag: "DeleteTopic"
              , contents: { topicDescription: t.topicDescription
                          , topicTyp: show t.topicTyp
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
              , contents: { topicDescription: t.topicDescription
                          , topicTyp: show t.topicTyp
                          }
              }

foreign import parseAssignTopic
"""
function parseAssignTopic(foreign) {
  return {
    topic : foreign.contents[1],
    slot : foreign.contents[0]
    }
  }
  """ :: Foreign -> Foreign


foreign import serializeAssignTopic
"""
function serializeAssignTopic(slot) {
  return function(topic){
    return { tag: "AssignTopic",
             contents:[
               slot,
               {
                 topicDescription: topic.topicDescription
                 , topicTyp: Prelude.show(showTopicType)(topic.topicTyp)
               }
             ]
           }
  }
}
""" :: Slot -> Topic -> Foreign

-------------------------
--| Entire AppState |--
-------------------------

type Timeslot = Tuple Slot Topic

type AppState = { topics :: [Topic]
                , rooms :: [Room]
                , blocks :: [Block]
                , timeslots :: M.Map Slot Topic
                }

type SanitizedTopic = { topicDescription :: String, topicTyp :: String }
type SanitizedSlot =  { room :: String, block :: String }
type SanitizedTimeslot = Tuple SanitizedSlot SanitizedTopic

type SanitizedAppState = { topics :: [SanitizedTopic]
                         , rooms :: [Room]
                         , blocks :: [String]
                         , timeslots :: [SanitizedTimeslot]
                         }

 --------------------
 --| Dummy Values |--
 --------------------

emptyState = {topics: [], rooms:[], blocks:[], timeslots: (M.empty) :: M.Map Slot Topic }

myRoom = Room {roomName: "Berlin", roomCapacity: 100}
myRoom1 = Room {roomName: "Hamburg", roomCapacity: 80}
myRoom2 = Room {roomName: "Köln", roomCapacity: 30}

myBlock = Block {
  blockDescription:"First",
  blockStartHours: 8,
  blockStartMinutes: 0,
  blockEndHours: 10,
  blockEndMinutes: 0}
myBlock1 = Block {
  blockDescription:"Second",
  blockStartHours: 8,
  blockStartMinutes: 0,
  blockEndHours: 10,
  blockEndMinutes: 0}

mySlot = Slot {room:myRoom, block:myBlock}
mySlot1 = Slot {room:myRoom1, block:myBlock1}

myTopic = Topic  {topicDescription:"Purescript is great", topicTyp:Workshop}
myTopic1 = Topic {topicDescription:"Reactive Design", topicTyp:Presentation}
myTopic2 = Topic {topicDescription:"Functional Javascript", topicTyp:Discussion}
myTopic3 = Topic {topicDescription:"Enemy of the State", topicTyp:Presentation}
myTopic4 = Topic {topicDescription:"Wayyyyyyy too long name for a Topic.", topicTyp:Workshop}
myTopic5 = Topic {topicDescription:"fix", topicTyp:Discussion}

myState1 = { topics: [myTopic, myTopic1, myTopic2, myTopic3, myTopic4, myTopic5]
           , rooms : [myRoom, myRoom1, myRoom2]
           , blocks : [myBlock, myBlock1]
           , timeslots: M.fromList [Tuple mySlot myTopic, Tuple mySlot1 myTopic1]
           }
