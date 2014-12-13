module Event.Stream.Input where

import Rx.JQuery
import Rx.Observable

import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Tuple

import Control.Monad.Eff
import Control.Monad.ST
import qualified Control.Monad.JQuery as J
import Debug.Trace
import DOM

import Render.Types
import Render.Main


foreign import readTopic
"""
function readTopic (){
  return { description : $('#topicInput').val(), typ: $('#typInput').val() };
}
""" :: forall eff. Eff( dom :: DOM | eff ) Foreign

foreign import readSlot
"""
function readSlot (){
  return { room: $('#roomInput').val(), time: parseFloat($('#timeInput').val(), 10) };
}
""" :: forall eff. Eff( dom :: DOM | eff ) Foreign

foreign import getDetail
"""
function getDetail (e){
  return function(){
    return e.originalEvent.detail;
    }
}
""" :: forall eff. J.JQueryEvent -> Eff( dom :: DOM | eff ) Foreign

parseTopic :: Foreign -> Either ForeignError Topic
parseTopic ft = do
  t <- read ft :: F Topic
  return t

parseSlot :: Foreign -> Either ForeignError Slot
parseSlot fs = do
  s <- read fs :: F Slot
  return s

parseTimeslot :: Foreign -> Foreign -> Either ForeignError Timeslot
parseTimeslot fs ft = do
  s <- parseSlot fs
  t <- parseTopic ft
  return $ Tuple s t
  
type Stream a h eff = Eff( dom :: DOM, trace :: Trace, st :: ST h | eff ) Unit

streams :: forall eff h. Stream AppState h eff
streams = do

  appSt <- newSTRef myState1
  renderApp <$> readSTRef appSt

  add <- J.select "#addBtn"
  emitter <- J.select ".app"

  onAdd  <- "click" `onAsObservable` add
  onSelect <- "selectTimeSlot" `onAsObservable` emitter

  subscribe onSelect (\e -> do
    ft <- getDetail e
    case parseTopic ft of
      Right t -> void $ modifySTRef appSt $ select t
      Left e -> trace $ show e
    renderApp <$> readSTRef appSt
    )

  subscribe onAdd (\_-> do
    t <- readTopic
    s <- readSlot
    case parseTimeslot s t of
      Right ts -> void $ modifySTRef appSt $ addTimeslot ts
      Left e -> trace $ show e
    renderApp <$> readSTRef appSt
    )

main = runST streams
