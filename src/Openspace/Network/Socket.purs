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
