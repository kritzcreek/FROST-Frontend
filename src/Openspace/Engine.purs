module Openspace.Engine where

import           Data.Array
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import           Data.Tuple
import           Openspace.Types


evalAction :: Action -> AppState -> AppState
evalAction (AddTopic t) as      = addTopic t as
evalAction (DeleteTopic t) as   = deleteTopic t as
evalAction (AddRoom r) as       = addRoom r as
evalAction (DeleteRoom r) as    = deleteRoom r as
evalAction (AddBlock b) as      = addBlock b as
evalAction (DeleteBlock b) as   = deleteBlock b as
evalAction (AssignTopic s t) as = addTimeslot s t as
evalAction (UnassignTopic t) as = let topicslotFilter = filter (\(Tuple _ t') -> t' /= t)
                                  in as {timeslots = M.fromList $ topicslotFilter (M.toList as.timeslots)}
evalAction (ReplayActions actions) _ = generateState actions
evalAction (ShowError e) as     = as
evalAction NOP as               = as

addTimeslot :: Slot -> Topic -> AppState -> AppState
addTimeslot s t as = let topicslotFilter = filter (\(Tuple s t') -> t' /= t)
                     in as { timeslots = M.insert s t $ M.fromList $ topicslotFilter (M.toList as.timeslots) }

deleteTimeslot :: Slot -> AppState -> AppState
deleteTimeslot s as = as { timeslots = M.delete s as.timeslots }

addTopic :: Topic -> AppState -> AppState
addTopic t as = as { topics = t:as.topics}

deleteTopic :: Topic -> AppState -> AppState
deleteTopic t as = let topicslotFilter = filter (\(Tuple s t') -> t' /= t)
                   in as { topics    = delete t as.topics
                         , timeslots = M.fromList $ topicslotFilter (M.toList as.timeslots)
                         }

addRoom :: Room -> AppState -> AppState
addRoom r as = as { rooms = r : as.rooms }

deleteRoom :: Room -> AppState -> AppState
deleteRoom r as = as { rooms = filter (\r' -> r' /= r ) as.rooms
                     , timeslots = M.fromList $ topicslotFilter (M.toList as.timeslots)}
  where topicslotFilter = filter (\(Tuple (Slot s) _) -> s.room /= r)

addBlock :: Block -> AppState -> AppState
addBlock b as = as { blocks = b : as.blocks }

deleteBlock :: Block -> AppState -> AppState
deleteBlock b as = as { blocks = filter (\b' -> b' /= b ) as.blocks
                      , timeslots = M.fromList $ topicslotFilter (M.toList as.timeslots)}
  where topicslotFilter = filter (\(Tuple (Slot s) _) -> s.block /= b)

generateState :: [Action] -> AppState
generateState as = foldl (flip evalAction) emptyState as
