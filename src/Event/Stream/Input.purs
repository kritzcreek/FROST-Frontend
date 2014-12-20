module Event.Stream.Input where

import Rx.JQuery
import Rx.Observable

import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Tuple hiding(zip)
import Data.Maybe

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
    return e.originalEvent.detail;
}
""" :: forall eff. J.JQueryEvent -> Foreign

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

streams :: forall h eff. Stream AppState h eff
streams = do

  appSt <- newSTRef myState1
  readSTRef appSt >>= renderApp
  renderMenu (show <$> topicTypes)

  menuEmitter              <- J.select "#menuContainer"
  onAddTopic               <- "addTopic" `onAsObservable` menuEmitter
  onDragOverTrash          <- "dragOverTrash" `onAsObservable` menuEmitter
  onDragLeaveTrash         <- "dragLeaveTrash" `onAsObservable` menuEmitter

  topicEmitter             <- J.select "#topicsContainer"
  onDragStartTopic         <- "dragStartTopic" `onAsObservable` topicEmitter
  onDragEndTopic           <- "dragEndTopic" `onAsObservable` topicEmitter

  gridEmitter              <- J.select "#gridContainer"
  onDragOverSlot           <- "dragOverSlot" `onAsObservable` gridEmitter
  onDragLeaveSlot          <- "dragLeaveSlot" `onAsObservable` gridEmitter

  let add = (\e -> do
                let ft = getDetail e
                case parseTopic ft of
                  Right t -> AddTopic t
                  Left e -> ShowError (show e)
            ) <$> onAddTopic
  
  let dragOverSlot = (\e -> case parseSlot (getDetail e) of
                         Right s -> AssignTopic s
                         Left e -> UnassignTopic
                     ) <$> onDragOverSlot

  let dragOverTrash = (const DeleteTopic) <$> onDragOverTrash

  -- HTML 5 fires dragLeave before dragEnd occurs
  -- TODO: Find a cleaner solution
  let dragLeave = delay 30 $ (const UnassignTopic) <$> (onDragLeaveSlot `merge` onDragLeaveTrash)
  
  let dragOver = dragLeave `merge` dragOverSlot `merge` dragOverTrash 
      
  let dragTopic =
        do (Right t) <- (parseTopic <<< getDetail) <$> onDragStartTopic
           action <- dragOver
           onDragEndTopic
           return $ action t

  let change = add `merge` dragTopic

  subscribe change (\a -> modifySTRef appSt (evalAction a) >>= renderApp )


main = streams
