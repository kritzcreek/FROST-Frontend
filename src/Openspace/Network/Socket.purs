module Openspace.Network.Socket where

import Control.Monad.Eff
import Data.Either
import Data.Foreign
import Rx.Observable


data Socket = Socket | EmptySocket -- TODO: remove the need for EmptySocket
data SocketError = SocketError

foreign import data Net :: !
foreign import data Message :: *

foreign import getSocket
"""
function getSocket(url){
    return new WebSocket(url);
}
""" :: String -> Socket

foreign import socketObserver
"""
function socketObserver(ws){
  return function (){
    return Rx.Observable.create (function (obs) {
      // Handle messages
      ws.onmessage = obs.onNext.bind(obs)
      //TODO: Handle ServerNotAvailable
      //ws.onerror = obs.onError.bind(obs)
      ws.onclose = obs.onCompleted.bind(obs)
      // Return way to unsubscribe
      return ws.close.bind(ws)
    })
  }
}
""" :: forall eff. Socket -> Eff (net :: Net | eff) (Observable Message)

foreign import parseMessage
"""
function parseMessage(msg){
  return JSON.parse(msg.data)
}
""" :: Message -> Foreign

foreign import emitAction
"""
function emitAction(socket){
  return function (action){
    return function(){
      if(socket.readyState == WebSocket.OPEN){ socket.send(JSON.stringify(action)) }
    }
  }
}
""" :: forall eff. Socket -> Foreign -> Eff (net :: Net | eff) Unit

foreign import getHost
"""
  function getHost() {
      return window.location.host;
  }
""" :: forall eff. Eff( net :: Net |eff ) String


foreign import emitRefresh
"""
function emitRefresh(socket){
  return function(){
    //UGLY HACK!
    if(socket.readyState == WebSocket.OPEN){
      socket.send(JSON.stringify({"tag":"RequestState","contents":[]}))
    }else{
      socket.onopen = function(){
        socket.send(JSON.stringify({"tag":"RequestState","contents":[]}))
      }
    }
  }
}
""" :: forall eff. Socket -> Eff ( net :: Net | eff ) Unit
