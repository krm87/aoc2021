module Day1  where

import Data.Array (snoc, take, (!!))
import Data.Maybe (maybe)
import Prelude (Unit, bind, identity, map, show, ($), (+), (<<<), (<>), (>), discard)
import Data.Int (fromString)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Foldable (sum)
import Utils

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "./day1.txt"
  log $ "Part 1: " <> (show $ countIncrements 0 <<< readNumbers $ text)
  log $ "Part 2: " <> (show $ countIncrements 0 <<< toWindow3 [] <<< readNumbers $ text)

readNumbers :: String -> Array Int
readNumbers = map (maybe 0 identity <<< fromString) <<< (split (Pattern "\n"))

countIncrements :: Int -> Array Int -> Int
countIncrements n [] = n
countIncrements n xs = if xs !! 1 > xs !! 0
  then countIncrements (n + 1) $ tailOrEmpty xs
  else countIncrements n $ tailOrEmpty xs

toWindow3 :: Array Int -> Array Int -> Array Int
toWindow3 acc [] = acc
toWindow3 acc [_] = acc
toWindow3 acc [_,_] = acc
toWindow3 acc xs = toWindow3 (snoc acc $ sum $ take 3 xs) $ tailOrEmpty xs