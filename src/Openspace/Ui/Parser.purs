module Openspace.Ui.Parser where


import Control.Monad.Eff.JQuery hiding (append)
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Tuple (Tuple(..))
import Openspace.Types
import Prelude
import Rx.Observable

foreign import getDetail :: JQueryEvent -> Foreign

type Parser a = Foreign -> F a

actionFromForeign :: forall a. Parser a -> (a -> Action) -> Foreign -> Action
actionFromForeign parser actionConstructor f =
  case parser f of
    Right a -> actionConstructor a
    Left e -> ShowError (show e)

parseAction :: Foreign -> F Action
parseAction fa = do
  a <- read fa :: F Action
  return a

parseTopic :: Foreign -> F Topic
parseTopic ft = do
  t <- read ft :: F Topic
  return t

parseRoom :: Foreign -> F Room
parseRoom fr = do
  r <- read fr :: F Room
  return r

parseBlock :: Foreign -> F Block
parseBlock fb = do
  b <- read fb :: F Block
  return b

parseSlot :: Foreign -> F Slot
parseSlot fs = do
  s <- read fs :: F Slot
  return s

parseTimeslot :: Foreign -> Foreign -> F Timeslot
parseTimeslot fs ft = do
  s <- parseSlot fs
  t <- parseTopic ft
  return $ Tuple s t
