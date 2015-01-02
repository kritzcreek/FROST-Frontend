module Openspace.Ui.Parser where

import Rx.Observable

import Openspace.Types

import Data.Foreign
import Data.Foreign.Class

import Data.Either
import Data.Tuple hiding (zip)
import Control.Monad.JQuery

foreign import getDetail
"""
function getDetail (e){
    return e.originalEvent.detail;
}
""" :: JQueryEvent -> Foreign

parseAction :: Foreign -> F Action
parseAction fa = do
  a <- read fa :: F Action
  return a

parseTopic :: Foreign -> F Topic
parseTopic ft = do
  t <- read ft :: F Topic
  return t

parseSlot :: Foreign -> F Slot
parseSlot fs = do
  s <- read fs :: F Slot
  return s

parseTimeslot :: Foreign -> Foreign -> F Timeslot
parseTimeslot fs ft = do
  s <- parseSlot fs
  t <- parseTopic ft
  return $ Tuple s t
