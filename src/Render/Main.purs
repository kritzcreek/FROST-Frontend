module Render.Main where

import Render.Types
import Data.Array
import Data.Maybe
import Data.Tuple
import Control.Monad.Eff

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


foreign import renderApp
"""function renderApp(app){
    React.render(
      React.createElement(MainApp, {appState: app}),
      document.getElementById('content')
    );
}
""" :: AppState -> Unit
