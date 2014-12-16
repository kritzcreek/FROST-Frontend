module Render.Main where

import Render.Types
import Data.Array
import Data.Maybe
import Data.Tuple
import Control.Monad.Eff

import DOM

addTimeslot :: Timeslot -> AppState -> AppState
addTimeslot ts as = { topics     : as.topics
                    , rooms      : as.rooms
                    , blocks     : as.blocks
                    , slots      : as.slots
                    , timeslots  : ts : as.timeslots
                    , selected   : as.selected
                    }

removeTimeslot :: Timeslot -> AppState -> AppState
removeTimeslot ts as = { topics     : as.topics
                       , rooms      : as.rooms
                       , blocks     : as.blocks
                       , slots      : as.slots
                       , timeslots  : delete ts as.timeslots
                       , selected   : as.selected
                       }

addTopic :: Topic -> AppState -> AppState
addTopic t as = { topics     : t:as.topics
                , rooms      : as.rooms
                , blocks     : as.blocks
                , slots      : as.slots
                , timeslots  : as.timeslots
                , selected   : as.selected
                }

removeTopic :: Topic -> AppState -> AppState
removeTopic t as = let topicslotFilter = filter (\(Tuple s t') -> t' /= t)
                   in { topics    : delete t as.topics
                      , rooms     : as.rooms
                      , blocks    : as.blocks
                      , slots     : as.slots
                      , timeslots : topicslotFilter as.timeslots
                      , selected  : as.selected
                      }

select :: Topic -> AppState -> AppState
select topic as = { topics     : as.topics
                  , rooms      : as.rooms
                  , blocks     : as.blocks
                  , slots      : as.slots
                  , timeslots  : as.timeslots
                  , selected   : Just topic
                  }

unselect :: AppState -> AppState
unselect as = { topics     : as.topics
              , rooms      : as.rooms
              , blocks     : as.blocks
              , slots      : as.slots
              , timeslots  : as.timeslots
              , selected   : Nothing :: Maybe Topic
              }

sanitizeTopic :: Topic -> SanitizedTopic
sanitizeTopic (Topic t) = { description : t.description
                          , typ         : (show t.typ)
                          }
sanitizeSlot :: Slot -> SanitizedSlot
sanitizeSlot (Slot s) = { room  : show s.room
                        , block : show s.block
                        }

sanitizeTimeslot :: Timeslot -> SanitizedTimeslot
sanitizeTimeslot (Tuple s t) = Tuple (sanitizeSlot s) (sanitizeTopic t)

sanitizeAppState :: AppState -> SanitizedAppState
sanitizeAppState as = { topics     : sanitizeTopic    <$> as.topics
                      , rooms      : as.rooms
                      , blocks     : as.blocks
                      , slots      : sanitizeSlot     <$> as.slots
                      , timeslots  : sanitizeTimeslot <$> as.timeslots
                      , selected   : sanitizeTopic    <$> as.selected
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
"""function renderMenu(){
    React.render(
      React.createElement(Menu, null),
      document.getElementById('menu')
    );
}
""" :: forall eff. Eff( dom::DOM | eff ) Unit

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

foreign import renderTimeslots
"""function renderTimeslots(timeslotsAndSelected){
    return function(){
      React.render(
          React.createElement(Timeslots, {timeslotsAndSelected: timeslotsAndSelected}),
          document.getElementById('timeslots')
        );
      }
    }
""" :: forall eff. Tuple [SanitizedTimeslot] (Maybe SanitizedTopic) -> Eff( dom::DOM | eff ) Unit

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
  renderTimeslots $ Tuple as'.timeslots as'.selected
  renderTopics    $ Tuple as'.topics    as'.selected
  renderGrid as'.rooms as'.blocks (makeGrid as')
