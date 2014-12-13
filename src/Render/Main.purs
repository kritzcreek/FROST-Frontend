module Render.Main where

import Render.Types
import Data.Array
import Control.Monad.Eff

showSlot :: Slot -> String
showSlot (Slot {room=r, time=t}) = "Raum " ++ r ++ " Zeitpunkt: " ++ show t

addTimeslot :: Timeslot -> AppState -> AppState
addTimeslot ts as = { timeslots: ts : as.timeslots }

removeTimeslot :: Timeslot -> AppState -> AppState
removeTimeslot ts as = { timeslots: delete ts as.timeslots }

foreign import renderApp
"""function renderApp(app){
    React.render(
      React.createElement(MainApp, {appState: app}),
      document.getElementById('content')
    );
}
""" :: AppState -> Unit
