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

foreign import getDetail
"""
function getDetail (e){
    return e.originalEvent.detail;
}
""" :: forall eff. J.JQueryEvent -> Foreign

foreign import getSocket
"""
function getSocket(url){
  return function(){
    return io(url);
  }
}
""" :: forall eff. String -> Eff (dom::DOM | eff) J.JQuery

foreign import socketObserver
"""
function socketObserver(channel){
  return function(socket){
    return function(){
      var __slice = [].slice;
      var subj;
      subj = new Rx.Subject();
      subj.callback = function(msg) {
       return subj.onNext(msg);
      };
      socket.on(channel, subj.callback);
      return subj;
    }
  }
}
""" :: forall eff. String -> J.JQuery -> Eff (dom :: DOM | eff) (Observable Foreign)

foreign import emitAction
"""
function emitAction(socket){
  return function (action){
    return function(){
      socket.emit('message', action)
    }
  }
}
""" :: forall eff. J.JQuery -> Foreign -> Eff (dom :: DOM | eff) Unit

parseAction :: Foreign -> Either ForeignError Action
parseAction fa = do
  a <- read fa :: F Action
  return a

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

  sockEmitter              <- getSocket "http://localhost:3000"
  onReceive                <- "message" `socketObserver` sockEmitter
  
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

  let receive = (\ f -> case parseAction f of
                    Right a -> a
                    Left e -> ShowError (show e)
                ) <$> onReceive

  let change = add `merge` dragTopic

  subscribe receive (\a -> do modifySTRef appSt (evalAction a) >>= renderApp)
  
  subscribe change (\a -> do modifySTRef appSt (evalAction a) >>= renderApp
                             emitAction sockEmitter (serialize a)
                   )
  
main = streams
