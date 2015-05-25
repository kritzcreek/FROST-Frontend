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
  (==) (Topic t1) (Topic t2) = t1.description == t2.description && t1.typ == t2.typ
  (/=) t1 t2 = not (t1 == t2)

instance foreignTopic :: IsForeign Topic where
    read val = do
      topic <- readProp "description" val :: F String
      typ   <- readProp "typ"   val :: F TopicType
      return $ Topic { description: topic, typ: typ }


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
  (==) (Block b1) (Block b2) = b1.description == b2.description
                               && b1.startHours == b2.startHours
                               && b1.startMinutes == b2.startMinutes
                               && b1.endHours == b2.endHours
                               && b1.endMinutes == b2.endMinutes

  (/=) b1 b2 = not (b1 == b2)

instance ordBlock :: Ord Block where
  compare (Block b1) (Block b2) = compare (hourDiff * 60 + minDiff) 0
    where hourDiff = b1.startHours - b2.startHours
          minDiff = b1.startMinutes - b2.startMinutes



instance foreignBlock :: IsForeign Block where
  read val = do
    description <- readProp "description" val
    startHours <- readProp "startHours" val
    startMinutes <- readProp "startMinutes" val
    endHours <- readProp "endHours" val
    endMinutes <- readProp "endMinutes" val
    return $ Block { description: description
                   , startHours: startHours
                   , startMinutes: startMinutes
                   , endHours: endHours
                   , endMinutes: endMinutes
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
                 description: topic.description
                 , typ: Prelude.show(showTopicType)(topic.typ)
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

 --------------------
 --| Dummy Values |--
 --------------------

emptyState = {topics: [], rooms:[], blocks:[], timeslots: (M.empty) :: M.Map Slot Topic }

myRoom = Room {name: "Berlin", capacity: 100}
myRoom1 = Room {name: "Hamburg", capacity: 80}
myRoom2 = Room {name: "Koeln", capacity: 30}

myBlock = Block {
  description:"First",
  startHours: 8,
  startMinutes: 0,
  endHours: 10,
  endMinutes: 0}
myBlock1 = Block {
  description:"Second",
  startHours: 8,
  startMinutes: 0,
  endHours: 10,
  endMinutes: 0}

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
           , timeslots: M.fromList [Tuple mySlot myTopic, Tuple mySlot1 myTopic1]
           }
