module Openspace.Network.Socket where

import Control.Monad.Eff
import Data.Foreign
import Data.Either
import Rx.Observable


data Socket = Socket | EmptySocket -- TODO: remove the need for EmptySocket
data SocketError = SocketError

foreign import data Net :: !

foreign import getSocket
"""
function getSocket(url){
    return io(url);
}
""" :: String -> Socket

socketObserver' _ EmptySocket = return empty
socketObserver' str sock = socketObserver str sock

foreign import socketObserver
"""
function socketObserver(address){
  return function(protocol){
    return function(openObserver) {
      var ws = new WebSocket(address, protocol);

      var observer = Rx.Observer.create(function (data) {
        if (ws.readyState === WebSocket.OPEN) { ws.send(data); }
      });

      // Handle the data
      var observable = Rx.Observable.create (function (obs) {
        // Handle open
        if (openObserver) {
          ws.onopen = function (e) {
            openObserver.onNext(e);
            openObserver.onCompleted();
          };
        }

        // Handle messages
        ws.onmessage = obs.onNext.bind(obs);
        ws.onerror = obs.onError.bind(obs);
        ws.onclose = obs.onCompleted.bind(obs);

        // Return way to unsubscribe
        return ws.close.bind(ws);
      });

      return Rx.Subject.create(observer, observable);
    }
  }
}
""" :: forall eff. String -> Socket -> Eff (net :: Net | eff) (Observable Foreign)

foreign import emitAction
"""
function emitAction(socket){
  return function (action){
    return function(){
      socket.emit('message', action)
    }
  }
}
""" :: forall eff. Socket -> Foreign -> Eff (net :: Net | eff) Unit

foreign import emitRefresh
"""
function emitRefresh(socket){
  return function(){
    socket.emit('state')
  }
}
""" :: forall eff. Socket -> Eff ( net :: Net | eff ) Unit
