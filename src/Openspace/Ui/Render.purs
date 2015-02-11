module Openspace.Ui.Render where

import qualified Data.Map as M
import Data.Tuple
import Data.Array
import Data.Maybe

import Control.Monad.Eff
import DOM

import Openspace.Types



sanitizeTopic :: Topic -> SanitizedTopic
sanitizeTopic (Topic t) = { topicDescription: t.topicDescription,
                            topicTyp: show t.topicTyp }

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
                         , blocks          : show <$> as.blocks
                         , timeslots       : sanitizeTimeslot <$> (M.toList as.timeslots)
                         }

findIn :: Room -> Block -> M.Map Slot Topic -> Maybe SanitizedTopic
findIn r b timeslots = M.lookup (Slot {block:b, room:r}) timeslots
                       <#> sanitizeTopic

makeGrid :: AppState -> [[Maybe SanitizedTopic]]
makeGrid as = (\r ->
                (\b -> findIn r b as.timeslots ) <$> as.blocks
              ) <$> as.rooms

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
  """function renderTopics(topics){
    return function(){
      React.render(
        React.createElement(Topics, {topics: topics}),
        document.getElementById('topics')
        )
      }
    }
    """ :: forall eff. [SanitizedTopic] -> Eff( dom::DOM | eff ) Unit

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
  renderTopics $ as'.topics
  renderGrid as.rooms as.blocks (makeGrid as)
