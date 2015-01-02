module Openspace.Ui.Emitter where

import Control.Monad.Eff

import Rx.JQuery
import Rx.Observable

import qualified Control.Monad.JQuery as J

import Data.Maybe
import Data.Tuple hiding(zip)
import Data.Traversable
import Data.Array (append)
import qualified Data.Map as M 

import DOM

type Emitter = Tuple String (Observable J.JQueryEvent)
type Emitters = M.Map String (Observable J.JQueryEvent)

mkObservable :: forall eff. J.JQuery -> String -> Eff( dom::DOM | eff) (Tuple String (Observable J.JQueryEvent))
mkObservable emitter event = (Tuple event) <$> (event `onAsObservable` emitter)

--getEmitters' :: forall eff. Tuple String [String] -> Eff( dom::DOM | eff) [Emitter]
--getEmitters' (Tuple container events) = sequence $ do c <- J.select container
--                                                      mkObservable c <$> events

--getEmitters :: forall eff. [Tuple String [String]] -> Eff( dom :: DOM | eff) Emitters
getEmitters = do
    menuEmitter <- J.select "#menuContainer"
    let menu = mkObservable menuEmitter <$> ["addTopic", "dragOverTrash", "dragLeaveTrash"]
                
    topicEmitter <- J.select "#topicsContainer"
    let topic = mkObservable topicEmitter <$> ["dragStartTopic", "dragEndTopic"]
       
    gridEmitter <- J.select "#gridContainer"
    let grid = mkObservable gridEmitter <$> ["dragOverSlot", "dragLeaveSlot"]
       
    M.fromList <$> (sequence $ menu `append` topic `append` grid)

emitterLookup :: Emitters -> String -> Observable J.JQueryEvent
emitterLookup es s =
  case M.lookup s es of
    Just ob -> ob
    Nothing -> empty
