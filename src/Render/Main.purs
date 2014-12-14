module Render.Main where

import Render.Types
import Data.Array
import Data.Maybe
import Data.Tuple
import Control.Monad.Eff

import DOM

addTimeslot :: Timeslot -> AppState -> AppState
addTimeslot ts as = { topics     : as.topics
                    , slots      : as.slots
                    , timeslots  : ts : as.timeslots
                    , selected   : as.selected
                    }

removeTimeslot :: Timeslot -> AppState -> AppState
removeTimeslot ts as = { topics     : as.topics
                       , slots      : as.slots
                       , timeslots  : delete ts as.timeslots
                       , selected   : as.selected
                       }

addTopic :: Topic -> AppState -> AppState
addTopic t as = { topics     : t:as.topics
                , slots      : as.slots
                , timeslots  : as.timeslots
                , selected   : as.selected
                }

removeTopic :: Topic -> AppState -> AppState
removeTopic t as = let topicslotFilter = filter (\(Tuple s t') -> t' /= t)
                   in { topics    : delete t as.topics
                      , slots     : as.slots
                      , timeslots : topicslotFilter as.timeslots
                      , selected  : as.selected
                      }

select :: Topic -> AppState -> AppState
select topic as = { topics     : as.topics
                  , slots      : as.slots
                  , timeslots  : as.timeslots
                  , selected   : Just topic
                  }

unselect :: AppState -> AppState
unselect as = { topics     : as.topics
              , slots      : as.slots
              , timeslots  : as.timeslots
              , selected   : Nothing :: Maybe Topic
              }

foreign import renderTopics
"""function renderTopics(topics){
  return function(){
    React.render(
      React.createElement(Topics, {topics: topics}),
      document.getElementById('topics')
    )
  }
}
""" :: forall eff. [Topic] -> Eff( dom::DOM | eff ) Unit

foreign import renderTimeslots
"""function renderTimeslots(timeslotsAndSelected){
  return function(){
    React.render(
        React.createElement(Timeslots, {timeslotsAndSelected: timeslotsAndSelected}),
        document.getElementById('timeslots')
      );
    }
  }
""" :: forall eff. Tuple [Timeslot] (Maybe Topic) -> Eff( dom::DOM | eff ) Unit

{- foreign import renderApp
"""function renderApp(app){
    React.render(
      React.createElement(MainApp, {appState: app}),
      document.getElementById('timeslots')
    );
}
""" :: forall eff. AppState -> Unit -}

renderApp :: forall eff. AppState -> Eff( dom::DOM | eff ) Unit
renderApp as = do
  renderTimeslots $ Tuple as.timeslots as.selected
  renderTopics as.topics
