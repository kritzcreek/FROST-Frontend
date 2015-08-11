module Openspace.Engine where


import Prelude
import           Data.Array
import           Data.Foldable
import qualified Data.Map as M
import           Data.Tuple (Tuple(..), snd)
import           Openspace.Types

filterMap :: forall k v. (Ord k) => (Tuple k v -> Boolean) -> M.Map k v -> M.Map k v
filterMap p = M.fromList <<< Data.List.filter p <<< M.toList

getSlot :: Tuple Slot _ -> { room :: Room, block :: Block }
getSlot (Tuple (Slot s) _) = s

evalAction :: Action -> AppState -> AppState
evalAction (AddTopic t) as      = addTopic t as
evalAction (DeleteTopic t) as   = deleteTopic t as
evalAction (AddRoom r) as       = addRoom r as
evalAction (DeleteRoom r) as    = deleteRoom r as
evalAction (AddBlock b) as      = addBlock b as
evalAction (DeleteBlock b) as   = deleteBlock b as
evalAction (AssignTopic s t) as = addTimeslot s t as
evalAction (UnassignTopic t) as = let topicslotFilter = (/=) t <<< snd
                                  in as { timeslots = filterMap topicslotFilter as.timeslots }
evalAction (ReplayActions actions) _ = generateState actions
evalAction (ShowError e) as     = as
evalAction NOP as               = as

addTimeslot :: Slot -> Topic -> AppState -> AppState
addTimeslot s t as = let topicslotFilter = (/=) t <<< snd
                     in as { timeslots = M.insert s t $ filterMap topicslotFilter as.timeslots }

addTopic :: Topic -> AppState -> AppState
addTopic t as = as { topics = t : as.topics}

deleteTopic :: Topic -> AppState -> AppState
deleteTopic t as = let topicslotFilter = (/=) t <<< snd
                   in as { topics    = delete t as.topics
                         , timeslots = filterMap topicslotFilter as.timeslots
                         }

addRoom :: Room -> AppState -> AppState
addRoom r as = as { rooms = r : as.rooms }

deleteRoom :: Room -> AppState -> AppState
deleteRoom r as = as { rooms = filter ((/=) r) as.rooms
                     , timeslots = filterMap topicslotFilter as.timeslots }
  where topicslotFilter ts = let slot = getSlot ts in r /= slot.room

addBlock :: Block -> AppState -> AppState
addBlock b as = as { blocks = b : as.blocks }

deleteBlock :: Block -> AppState -> AppState
deleteBlock b as = as { blocks = filter ((/=) b) as.blocks
                      , timeslots = filterMap topicslotFilter as.timeslots }
  where topicslotFilter ts = let slot = getSlot ts in b /= slot.block

generateState :: Array Action -> AppState
generateState as = foldl (flip evalAction) emptyState as
