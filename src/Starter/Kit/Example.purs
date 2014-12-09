module Starter.Kit.Example where

import Data.Array
import Data.Maybe

-- This module defines a single function diffs, which
-- returns the differences for an increasing array.
--
-- If the input is not increasing, it returns Nothing.
--
-- This contrived example was chosen to demonstrate the use
-- of the Data.Array and Data.Maybe standard libraries.

import Data.Tuple

inOrder :: forall a. (Ord a) => a -> a -> Tuple a a
inOrder a1 a2 | a1 < a2 = Tuple a1 a2
inOrder a1 a2 = Tuple a2 a1

diffs :: [Number] -> Maybe [Number]
diffs [] = return []
diffs [_] = return []
diffs (x : tail@(x' : xs)) | x <= x' = do
  tailDiffs <- diffs tail
  return (x' - x : tailDiffs)
diffs _ = Nothing
