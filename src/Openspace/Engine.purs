module Openspace.Engine where

import Openspace.Types
import Data.Maybe
import qualified Data.Map as M
import Data.Tuple
import Data.Array


evalAction :: Action -> AppState -> AppState
evalAction (AddTopic t) as      = addTopic t as
evalAction (DeleteTopic t) as   = removeTopic t as
evalAction (AssignTopic s t) as = addTimeslot s t as
evalAction (UnassignTopic t) as = let topicslotFilter = filter (\(Tuple _ t') -> t' /= t)
                                  in as {timeslots = M.fromList $ topicslotFilter (M.toList as.timeslots)}
evalAction (ShowError e) as     = as
evalAction NOP as               = as

addTimeslot :: Slot -> Topic -> AppState -> AppState
addTimeslot s t as = as { timeslots = M.insert s t as.timeslots }

removeTimeslot :: Slot -> AppState -> AppState
removeTimeslot s as = as { timeslots = M.delete s as.timeslots }

addTopic :: Topic -> AppState -> AppState
addTopic t as = as { topics = t:as.topics}

removeTopic :: Topic -> AppState -> AppState
removeTopic t as = let topicslotFilter = filter (\(Tuple s t') -> t' /= t)
                   in as { topics    = delete t as.topics
                         , timeslots = M.fromList $ topicslotFilter (M.toList as.timeslots)
                         }
