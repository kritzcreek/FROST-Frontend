module Event.Stream.Input where

import Control.Monad.Eff

import Rx.JQuery
import Rx.Observable

import Control.Monad.JQuery
import Debug.Trace

streams = do
  room   <- select "#roomInput"
  time   <- select "#timeInput"
  topic  <- select "#topicInput"
  typ    <- select "#typInput"
  submit <- select "#submitBtn"
  output <- select "#output"

  roomInput   <- "input" `onAsObservable` room
  timeInput   <- "input" `onAsObservable` time
  topicInput  <- "input" `onAsObservable` topic
  typInput    <- "input" `onAsObservable` typ
  submitClick <- "click" `onAsObservable` submit

  let roomTime = merge roomInput timeInput
  subscribe roomTime (\e -> trace e)
