module Day6 where

import Data.Traversable (sum)
import Data.Tuple (Tuple(..))
import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (foldl, head)
import Data.HashMap (toArrayBy)
import Data.HashMap as HM
import Data.Long (Long, fromInt)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils (tailOrEmpty, toIntArray)

type Amount = Long
type Days = Int
type AgeBin = Int

day6 :: Int -> Effect Unit
day6 days = do
  text <- readTextFile UTF8 "./src/Day6/day6_input.txt"
  case toIntArray $ split (Pattern ",") text of
    Just initial -> do
      let hashmap = binInts initial HM.empty
      let result = hashLoop hashmap days
      log $ show $ sum $ HM.values result
    Nothing -> log $ "Unable to parse input"

binInts :: Array Int -> HM.HashMap AgeBin Long -> HM.HashMap AgeBin Long
binInts [] m = m
binInts xs map = binInts (tailOrEmpty xs) newmap
    where
        h = headOrDefault xs 
        newmap = HM.insertWith (+) h (fromInt 1) map

headOrDefault :: Array Int -> Int
headOrDefault xs = case head xs of
                    Just x -> x
                    Nothing -> 0

hashLoop :: HM.HashMap AgeBin Amount -> Days -> HM.HashMap AgeBin Amount
hashLoop p i = tailRec go {population: p, day: i}
  where
  go :: _ -> Step _ (HM.HashMap AgeBin Amount)
  go {population: p', day: 0} = Done p'
  go {population: p', day: i'} = Loop {population: (ageHashPopulation p'), day: (i' - 1)}

ageHashPopulation :: HM.HashMap AgeBin Amount -> HM.HashMap AgeBin Amount
ageHashPopulation h = foldl process HM.empty $ toArrayBy (\ k v -> Tuple k v) h

process :: HM.HashMap Int Long -> Tuple AgeBin Amount -> HM.HashMap Int Long
process acc (Tuple 0 count) = HM.insertWith (+) 6 count $ (HM.insertWith (+) 8 count acc)
process acc (Tuple x count) = HM.insertWith (+) (x - 1) count acc