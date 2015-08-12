module Openspace.Network.Socket where

import Control.Monad.Eff
import Data.Either
import Data.Foreign
import Rx.Observable
import Prelude

data Socket = Socket | EmptySocket -- TODO: remove the need for EmptySocket
data SocketError = SocketError

foreign import data Net :: !

foreign import data Message :: *

foreign import getSocket :: String -> Socket

foreign import socketObserver :: forall eff. Socket -> Eff (net :: Net | eff) (Observable Message)

foreign import parseMessage :: Message -> Foreign

foreign import emitAction :: forall eff. Socket -> Foreign -> Eff (net :: Net | eff) Unit

foreign import getSocketUrl :: forall eff. Eff( net :: Net |eff ) String

foreign import emitRefresh :: forall eff. Socket -> Eff ( net :: Net | eff ) Unit
