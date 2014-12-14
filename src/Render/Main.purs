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

sanitizeTopic :: Topic -> SanitizedTopic
sanitizeTopic (Topic t) = { description : t.description
                          , typ         : (show t.typ)
                          }

sanitizeAppState :: AppState -> SanitizedAppState
sanitizeAppState as = { topics     : sanitizeTopic <$> as.topics
                      , slots      : as.slots
                      , timeslots  : as.timeslots
                      , selected   : (sanitizeTopic <$> as.selected) :: Maybe SanitizedTopic
                      }

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
""" :: forall eff. Tuple [Timeslot] (Maybe SanitizedTopic) -> Eff( dom::DOM | eff ) Unit

renderApp :: forall eff. AppState -> Eff( dom::DOM | eff ) Unit
renderApp as = do
  let as' = sanitizeAppState as
  renderTimeslots $ Tuple as'.timeslots as'.selected
  renderTopics    $ Tuple as'.topics    as'.selected
