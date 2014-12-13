module Render.Main where

import Render.Types
import Data.Array
import Data.Maybe
import Control.Monad.Eff

showSlot :: Slot -> String
showSlot (Slot {room=r, time=t}) = "Raum " ++ r ++ " Zeitpunkt: " ++ show t

addTimeslot :: Timeslot -> AppState -> AppState
addTimeslot ts as = { timeslots: ts : as.timeslots, selected: as.selected }

removeTimeslot :: Timeslot -> AppState -> AppState
removeTimeslot ts as = { timeslots: delete ts as.timeslots, selected: as.selected }

select :: Topic -> AppState -> AppState
select topic as = {timeslots: as.timeslots, selected: Just topic}

foreign import renderApp
"""function renderApp(app){
    React.render(
      React.createElement(MainApp, {appState: app}),
      document.getElementById('content')
    );
}
""" :: AppState -> Unit
