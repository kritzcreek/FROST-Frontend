module Openspace.Ui.Render where

import           Control.Apply
import           Control.Monad.Eff
import           DOM
import           Data.Array
import           Data.Function
import qualified Data.Map as M
import           Data.Maybe
import           Data.Tuple
import           Openspace.Types

type Grid = [[Maybe SanitizedTopic]]
type SanitizedTopic = { topicDescription :: String, topicTyp :: String }

sanitizeTopic :: Topic -> SanitizedTopic
sanitizeTopic (Topic t) = { topicDescription: t.topicDescription,
                            topicTyp: show t.topicTyp }

findIn :: Room -> Block -> M.Map Slot Topic -> Maybe SanitizedTopic
findIn r b timeslots = M.lookup (Slot {block:b, room:r}) timeslots
                       <#> sanitizeTopic

makeGrid :: AppState -> Grid
makeGrid as = (\r ->
                (\b -> findIn r b as.timeslots ) <$> (sort as.blocks)
              ) <$> as.rooms

foreign import renderMenu
  """
  function renderMenu(topicTypes){
    return function(){
      React.render(
        React.createElement(Menu, {topicTypes: topicTypes}),
        document.getElementById('menu')
        );
    }
  }
  """ :: forall eff. [String] -> Eff( dom::DOM | eff ) Unit

foreign import renderTopicsImpl
  """
  function renderTopicsImpl(topics){
    return function(){
      React.render(
        React.createElement(Topics, {topics: topics}),
        document.getElementById('topics')
        )
      }
    }
    """ :: forall eff. [SanitizedTopic] -> Eff( dom::DOM | eff ) Unit

foreign import renderGridImpl
  """
  function renderGridImpl(rooms, blocks, grid){
    return function(){
      React.render(
        React.createElement(Grid, {rooms: rooms, blocks: blocks, grid: grid}),
          document.getElementById('grid')
        );
    }
  }
  """ :: forall eff. Fn3 [Room] [Block] Grid (Eff( dom::DOM | eff ) Unit)

renderTopics :: forall eff. AppState -> Eff( dom::DOM | eff ) Unit
renderTopics as = renderTopicsImpl (sanitizeTopic <$> filter topicNotInGrid as.topics)
  where topicNotInGrid t = elemIndex t (M.values as.timeslots) == -1

renderGrid :: forall eff. AppState -> Eff( dom::DOM | eff ) Unit
renderGrid as = runFn3 renderGridImpl as.rooms (sort as.blocks) (makeGrid as)

renderApp :: forall eff. AppState -> Eff( dom::DOM | eff ) Unit
renderApp as = renderTopics as *> renderGrid as
