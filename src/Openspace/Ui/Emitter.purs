module Openspace.Ui.Emitter where

import           Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import           Control.Plus
import           Data.Array (append)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Traversable
import Data.Tuple hiding(zip)
import           Rx.JQuery
import           Rx.Observable

import           DOM

type Emitter = Tuple String (Observable J.JQueryEvent)
type Emitters = M.Map String (Observable J.JQueryEvent)

mkObservable :: forall eff. J.JQuery -> String -> Eff(dom :: DOM | eff) (Tuple String (Observable J.JQueryEvent))
mkObservable emitter event = (Tuple event) <$> (event `onAsObservable` emitter)

getEmitters :: forall eff. Eff(dom :: DOM | eff) Emitters
getEmitters = do
    menuEmitter <- J.select "#menuContainer"
    let menu = mkObservable menuEmitter <$> ["addTopic", "dragOverTrash"
                                            , "dragLeaveTrash"]

    topicEmitter <- J.select "#topicsContainer"
    let topic = mkObservable topicEmitter <$> ["dragStartTopic", "dragEndTopic"]

    gridEmitter <- J.select "#gridContainer"
    let grid = mkObservable gridEmitter <$> [ "dragOverSlot", "dragLeaveSlot"
                                            , "addRoom", "deleteRoom"
                                            , "addBlock", "deleteBlock"
                                            , "dragStartGridTopic", "dragEndGridTopic"
                                            , "removeBlock", "removeRoom"]

    M.fromList <$> (sequence $ menu `append` topic `append` grid)

emitterLookup :: Emitters -> String -> Observable J.JQueryEvent
emitterLookup es s =
  case M.lookup s es of
    Just ob -> ob
    Nothing -> empty
