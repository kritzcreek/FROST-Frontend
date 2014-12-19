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

  menuEmitter     <- J.select "#menuContainer"
  topicEmitter    <- J.select "#topicsContainer"
  gridEmitter     <- J.select "#gridContainer"

  onAddTopic               <- "addTopic" `onAsObservable` menuEmitter
  onRemoveTopic            <- "removeTopic" `onAsObservable` menuEmitter
  onDragStartTopic         <- "dragStartTopic" `onAsObservable` topicEmitter
  onDragEndTopic           <- "dragEndTopic" `onAsObservable` topicEmitter
  onDragOverSlot           <- "dragOverSlot" `onAsObservable` gridEmitter
  onDragOverTrash          <- "dragOverTrash" `onAsObservable` menuEmitter

  let add = (\e -> do
                let ft = getDetail e
                case parseTopic ft of
                  Right t -> AddTopic t
                  Left e -> ShowError (show e)
            ) <$> onAddTopic

  let delete = (\e -> do
                   let ft = getDetail e
                   case parseTopic ft of
                     Right t -> DeleteTopic t
                     Left e  -> ShowError (show e)
               ) <$> onRemoveTopic

  let dragOverSlot = (\e ->
                       case parseSlot (getDetail e) of
                         Right s -> AssignTopic s
                         Left e -> UnassignTopic
                     ) <$> onDragOverSlot

  let dragOverTrash = (const DeleteTopic) <$> onDragOverTrash
  
  let dragOver = dragOverSlot `merge` dragOverTrash
      
  let dragTopic =
        do  (Right t) <- (parseTopic <<< getDetail) <$> onDragStartTopic
            action <- dragOver
            onDragEndTopic
            return $ action t

  let change = add `merge` delete `merge` dragTopic

  subscribe change (\a -> do
                          (modifySTRef appSt $ evalAction a) >>= renderApp
                   )


main = runST streams
