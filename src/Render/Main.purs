module Render.Main where

import Render.Types
import Data.Array
import Data.Maybe
import qualified Data.Map as M
import Data.Tuple
import Control.Monad.Eff

import Debug.Trace
import DOM

evalAction :: Action -> AppState -> AppState
evalAction (SelectTopic t) as   = selectTopic t as
evalAction (AddTopic t) as      = addTopic t as
evalAction (DeleteTopic t) as   = removeTopic t as
evalAction (AssignTopic t s) as = addTimeslot s t as
evalAction (UnassignTopic t) as = let topicslotFilter = filter (\(Tuple _ t') -> t' /= t)
                                  in as {timeslots = M.fromList $ topicslotFilter (M.toList as.timeslots)}
evalAction (ShowError e) as     = as
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

selectTopic :: Topic -> AppState -> AppState
selectTopic topic as = as { selectedTopic = Just topic }

unselectTopic :: AppState -> AppState
unselectTopic as = as { selectedTopic = Nothing :: Maybe Topic }

sanitizeTopic :: Topic -> SanitizedTopic
sanitizeTopic (Topic t) = t { typ = show t.typ }

sanitizeSlot :: Slot -> SanitizedSlot
sanitizeSlot (Slot s) = { room  : show s.room
                        , block : show s.block
                        }

sanitizeTimeslot :: Timeslot -> SanitizedTimeslot
sanitizeTimeslot (Tuple s t) = Tuple (sanitizeSlot s) (sanitizeTopic t)

sanitizeAppState :: AppState -> SanitizedAppState
sanitizeAppState as = let topicNotInGrid t = (filter (\t' -> t == t')(M.values as.timeslots)) == []
                      in { topics          : sanitizeTopic    <$> filter topicNotInGrid as.topics
                         , rooms           : as.rooms
                         , blocks          : as.blocks
                         , slots           : sanitizeSlot     <$> as.slots
                         , timeslots       : sanitizeTimeslot <$> (M.toList as.timeslots)
                         , selectedTopic   : sanitizeTopic    <$> as.selectedTopic
                         }

findIn :: Room -> Block -> [SanitizedTimeslot] -> Maybe SanitizedTopic
findIn r b tss = let filteredtss = filter (\(Tuple s _) -> s.room == show r && s.block == show b) tss
                 in case filteredtss of
                  [] -> Nothing
                  [(Tuple _ t)] -> Just t
                  _ -> Nothing

makeGrid :: SanitizedAppState -> [[Maybe SanitizedTopic]]
makeGrid as' = (\r -> (\b -> findIn r b as'.timeslots ) <$> as'.blocks ) <$> as'.rooms

foreign import renderMenu
"""function renderMenu(topicTypes){
  return function(){
    React.render(
      React.createElement(Menu, {topicTypes: topicTypes}),
      document.getElementById('menu')
      );
  }
}
""" :: forall eff. [String] -> Eff( dom::DOM | eff ) Unit

foreign import renderTopics
"""function renderTopics(topicsAndSelected){
  return function(){
    React.render(
      React.createElement(Topics, {topicsAndSelected: topicsAndSelected}),
      document.getElementById('topics')
      )
    }
  }
  """ :: forall eff. Tuple [SanitizedTopic] (Maybe SanitizedTopic) -> Eff( dom::DOM | eff ) Unit

foreign import renderGrid
"""
function renderGrid(rooms){
  return function(blocks){
      return function(grid){
        return function(){
        React.render(
          React.createElement(Grid, {rooms: rooms, blocks: blocks, grid: grid}),
          document.getElementById('grid')
        );
      }
    }
  }
}

""" :: forall eff. [Room] -> [Block] -> [[Maybe SanitizedTopic]] -> Eff( dom::DOM | eff ) Unit

renderApp :: forall eff. AppState -> Eff( dom::DOM | eff ) Unit
renderApp as = do
  let as' = sanitizeAppState as
  renderTopics $ Tuple as'.topics as'.selectedTopic
  renderGrid as'.rooms as'.blocks (makeGrid as')
