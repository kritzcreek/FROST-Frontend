module Openspace.Ui.Stream where

import Rx.Observable
import Data.Either
import Control.Monad.Eff
import Control.Monad.ST
import DOM

import Openspace.Ui.Render
import Openspace.Ui.Emitter
import Openspace.Ui.Parser
import Openspace.Types
import Openspace.Engine
import Openspace.Network.Socket

netStream :: forall eff. Socket -> Eff( net :: Net | eff ) (Observable Action)
netStream socket = do
  onReceive <- "message" `socketObserver'` socket
  return $ (\ f -> case parseAction f of
               Right a -> a
               Left e -> ShowError (show e)
           ) <$> onReceive

uiStream :: forall eff. Eff( dom :: DOM | eff ) (Observable Action)
uiStream = do
  emitters <- getEmitters
  let lookup = emitterLookup emitters
  let add = (\e -> do
                let ft = getDetail e
                case parseTopic ft of
                  Right t -> AddTopic t
                  Left e -> ShowError (show e)
            ) <$> lookup "addTopic"

  let dragOverSlot = (\e -> case parseSlot (getDetail e) of
                         Right s -> AssignTopic s
                         Left e -> UnassignTopic
                     ) <$> lookup "dragOverSlot"

  let dragOverTrash = (const DeleteTopic) <$> (lookup "dragOverTrash")

  -- HTML 5 fires dragLeave before dragEnd occurs
  -- TODO: Find a cleaner solution
  let dragLeave = delay 30 $ (const UnassignTopic) <$> ((lookup "dragLeaveSlot") `merge` (lookup "dragLeaveTrash"))

  let dragOver = dragLeave `merge` dragOverSlot `merge` dragOverTrash

  let dragTopic = do (Right t) <- (parseTopic <<< getDetail) <$> lookup "dragStartTopic"
                     action <- dragOver
                     lookup "dragEndTopic"
                     return $ action t
  return $ add `merge` dragTopic

main = do
  -- TODO: getSocket :: Either SockErr Socket
  -- let sockEmitter = getSocket "http://localhost:3000"
  --| until Websocket support is given by the server
  let sockEmitter = EmptySocket
  -- Initial State
  appSt <- newSTRef myState1
  -- Initial Render
  renderMenu (show <$> topicTypes)
  readSTRef appSt >>= renderApp
  -- Observable Action
  ui  <- uiStream
  net <- netStream sockEmitter
  -- Broadcast the UI Observable
  unwrap $ (\a -> emitAction sockEmitter (serialize a)) <$> ui
  -- Evaluate Action Observables
  let actions = ui `merge` net
  subscribe actions (\a -> modifySTRef appSt (evalAction a) >>= renderApp)
