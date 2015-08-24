module Openspace.Ui.Emitter where

import           Control.Monad.Eff
import qualified Control.Monad.Eff.JQuery as J
import           Control.Plus
import           DOM
import           Data.List (toList)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid ()
import           Data.Traversable
import           Data.Tuple
import           Prelude
import           Rx.Observable

foreign import onAsObservable :: forall eff. String -> J.JQuery -> Eff(dom :: DOM | eff) (Observable J.JQueryEvent)

type Emitter = Tuple String (Observable J.JQueryEvent)
type Emitters = M.Map String (Observable J.JQueryEvent)

mkObservable :: forall eff. J.JQuery -> String -> Eff(dom :: DOM | eff) (Tuple String (Observable J.JQueryEvent))
mkObservable emitter event = (Tuple event) <$> (event `onAsObservable` emitter)

getEmitters :: forall eff. Eff(dom :: DOM | eff) Emitters
getEmitters = do
    menuEmitter <- J.select "#menuContainer"
    let menu = mkObservable menuEmitter <$> ["addTopic", "dragOverTrash"
                                            , "dragLeaveTrash", "addRoom"
                                            , "addBlock"]

    topicEmitter <- J.select "#topicsContainer"
    let topic = mkObservable topicEmitter <$> ["dragStartTopic", "dragEndTopic"]

    gridEmitter <- J.select "#gridContainer"
    let grid = mkObservable gridEmitter <$> [ "dragOverSlot", "dragLeaveSlot"
                                            ,  "deleteRoom" , "deleteBlock"
                                            , "dragStartGridTopic", "dragEndGridTopic"
                                            , "removeBlock", "removeRoom"]

    M.fromList <<< toList <$> (sequence $ menu <> topic <> grid)

emitterLookup :: Emitters -> String -> Observable J.JQueryEvent
emitterLookup es s =
  case M.lookup s es of
    Just ob -> ob
    Nothing -> empty
