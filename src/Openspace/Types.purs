module Openspace.Types where

import Data.Tuple
import Data.Maybe
import qualified Data.Map as M
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Index

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
-- TODO: Find a proper way to enumerate Union Datatypes
topicTypes = [Discussion, Presentation, Workshop]

--------------
--| Topics |--
--------------

newtype Topic = Topic { topic :: String, typ :: TopicType }

instance eqTopic :: Eq Topic where
  (==) (Topic t1) (Topic t2) = t1.topic == t2.topic && t1.typ == t2.typ
  (/=) (Topic t1) (Topic t2) = t1.topic /= t2.topic || t1.typ /= t2.typ

instance foreignTopic :: IsForeign Topic where
    read val = do
      topic <- readProp "topic" val :: F String
      typ   <- readProp "typ"   val :: F TopicType
      return $ Topic {topic: topic, typ: typ}


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
type Timerange = { start :: String, end :: String }

newtype Block = Block { description :: String
                      , range       :: Timerange }

instance showBlock :: Show Block where
  show (Block b) = b.description

instance eqBlock :: Eq Block where
  (==) (Block b1) (Block b2) = b1.description == b2.description
                               && b1.range.start == b2.range.start
                               && b1.range.end == b2.range.end
  (/=) b1 b2 = not (b1 == b2)

instance foreignBlock :: IsForeign Block where
  read val = do
    description <- readProp "description" val
    start <- prop "range" val >>= readProp "start"
    end <- prop "range" val >>= readProp "end"
    return $ Block {description: description
                   , range:{ start: start
                           , end: end }
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
  Right "ShowError" -> do
    m <- readProp "message" val :: F String
    return $ ShowError m
  Left e -> Right $ ShowError (show e)

class AsForeign a where
  serialize :: a -> Foreign

instance actionAsForeign :: AsForeign Action where
  serialize (AddTopic (Topic t)) =
    toForeign { tag: "AddTopic"
              , contents: { topic: t.topic
                          , typ: show t.typ
                          }
              }

  serialize (DeleteTopic (Topic t)) =
    toForeign { tag: "DeleteTopic"
              , contents: { topic: t.topic
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
              , contents: { topic: t.topic
                          , typ: show t.typ
                          }
              }

foreign import parseAssignTopic """
  function parseAssignTopic(foreign) {
  return {
    topic : foreign.contents[1],
    slot : foreign.contents[0]
    }
  }
""" :: Foreign -> Foreign


foreign import serializeAssignTopic """
  function serializeAssignTopic(slot) {
  return function(topic){
    return { tag: "AssignTopic",
             contents:[
               slot,
               {
                 topic: topic.topic
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

type SanitizedTopic = { topic :: String, typ :: String }
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

myRoom = Room {name: "Berlin", capacity: 100}
myRoom1 = Room {name: "Hamburg", capacity: 80}
myRoom2 = Room {name: "Köln", capacity: 30}

myBlock = Block { description:"First", range:{ start: "8:00am", end: "10:00am"} }
myBlock1 = Block { description:"Second", range:{ start: "10:00am", end: "12:00am"} }

mySlot = Slot {room:myRoom, block:myBlock}
mySlot1 = Slot {room:myRoom1, block:myBlock1}

myTopic = Topic  {topic:"Purescript is great", typ:Workshop}
myTopic1 = Topic {topic:"Reactive Design", typ:Presentation}
myTopic2 = Topic {topic:"Functional Javascript", typ:Discussion}
myTopic3 = Topic {topic:"Enemy of the State", typ:Presentation}
myTopic4 = Topic {topic:"Wayyyyyyy too long name for a Topic.", typ:Workshop}
myTopic5 = Topic {topic:"fix", typ:Discussion}

myState1 = { topics: [myTopic, myTopic1, myTopic2, myTopic3, myTopic4, myTopic5]
           , rooms : [myRoom, myRoom1, myRoom2]
           , blocks : [myBlock, myBlock1]
           , timeslots: M.fromList [Tuple mySlot myTopic, Tuple mySlot1 myTopic1]
           }
