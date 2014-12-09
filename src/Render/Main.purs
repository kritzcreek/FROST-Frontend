module Render.Main where

import Render.Types
import Data.Tuple
import Data.Tuple.Nested
import Data.Array
import Control.Monad.Eff
import Control.Monad.ST

showSlot :: Slot -> String
showSlot (Slot {room=r, time=t}) = "Raum " ++ r ++ " Zeitpunkt: " ++ show t

addTopic :: Slot -> Topic -> AppState -> AppState
addTopic s t as = { timeslots: (s /\ t) : as.timeslots }

removeTopic :: Slot -> Topic -> AppState -> AppState
removeTopic s t as = { timeslots: delete (s /\ t) as.timeslots }

render :: Number -> Unit
render _ = renderApp $ addTopic mySlot myTopic emptyState

foreign import renderApp
"""function renderApp(app){
    React.render(
      React.createElement(MainApp, {appState: app}),
      document.getElementById('content')
    );
}
""" :: AppState -> Unit
