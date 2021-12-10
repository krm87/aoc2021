module Day2  where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude

import Data.Array (foldl, index, mapMaybe)
import Data.Int (fromString)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils (readLines)

type Position =
  { depth :: Int
  , horizontal :: Int
  , aim :: Int
  }

data Navigation 
  = Forward Int
  | Down Int
  | Up Int

main :: Effect Unit
main = do
  let init = {depth: 0, horizontal: 0, aim: 0}
  text <- readTextFile UTF8 "./day2.txt"
  log $ "Part 1: " <> (show <<< setSail navigator init <<< mapMaybe stringToNavigation <<< readLines $ text)
  log $ "Part 2: " <> (show <<< setSail navigatorNew init <<< mapMaybe stringToNavigation <<< readLines $ text)

stringToNavigation :: String -> Maybe Navigation
stringToNavigation input = pairToNavigation =<< lineToPair input

lineToPair :: String -> Maybe (Tuple Int String)
lineToPair line= do
  let values = split (Pattern " ") line
  index0 <- index values 0
  index1 <- index values 1
  number <- fromString index1
  pure (Tuple number index0)

pairToNavigation :: Tuple Int String -> Maybe Navigation
pairToNavigation (Tuple x c) | c == "forward" = Just (Forward x)
                              | c == "down" = Just (Down x)
                              | c == "up" = Just (Up x)
                              | otherwise = Nothing

setSail :: (Position -> Navigation -> Position) -> Position -> Array Navigation -> Position
setSail nav pos xs = foldl nav pos xs

navigator :: Position -> Navigation -> Position
navigator pos (Forward x) = pos {horizontal = pos.horizontal + x}
navigator pos (Down x) = pos {depth = pos.depth + x}
navigator pos (Up x) = pos {depth = pos.depth - x}

navigatorNew :: Position -> Navigation -> Position
navigatorNew pos (Forward x) = pos {horizontal = pos.horizontal + x, depth = pos.depth + pos.aim * x}
navigatorNew pos (Down x) = pos {aim = pos.aim + x}
navigatorNew pos (Up x) = pos {aim = pos.aim - x}
