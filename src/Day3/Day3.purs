module Day3 where

import Data.Array (foldl, mapMaybe, (!!))
import Data.CodePoint.Unicode (decDigitToInt)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.String.CodeUnits (toCharArray)
import Matrix (Matrix, columns, fromArray, getColumn, getRow, height, rows, width)
import Prelude

import Data.Traversable (sequence, sum)
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Utils (readLines)

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "./day3.txt"
  case charGridToIntGrid <<< linesToCharGrid $ readLines text of
    Just grid -> do
      case fromArray grid of
        Just matrix -> do
          let gammaBits = map (calculateBit $ height matrix) (sumColumns matrix)
          let epsilonBits = map flipBit gammaBits
          let gamma = toInteger gammaBits
          let epsilon = toInteger epsilonBits
          log $ "Gamma bits: " <> show gammaBits
          log $ "Epsilon bits: " <> show epsilonBits
          log $ "Power rating: " <> show (gamma * epsilon)

          case filterMatrix matrix true 0 of
            Just oxygenBits -> do 
              log $ "Oxygen bits: " <> show oxygenBits
              case filterMatrix matrix false 0 of
                Just co2Bits -> do
                  let oxygen = toInteger oxygenBits
                  let co2 = toInteger co2Bits
                  log $ "Co2 bits: " <> show co2Bits
                  log $ "Life support rating: " <> show (oxygen * co2)
                Nothing -> log "No co2 rate found" 
            Nothing -> log "No oxygen rate found" 
        Nothing -> log $ "Invalid matrix dimensions"
    Nothing -> log $ "Grid contains invalid digits"

filterMatrix :: Matrix Int -> Boolean -> Int -> Maybe (Array Int)
filterMatrix m _ i | i == width m = getRow 0 m
                   | height m == 1 = getRow 0 m
filterMatrix m b i = do
  col <- getColumn i m
  let most = calculateBit (height m) (sum col)
  let bit = if b then most else flipBit most
  matrix <- fromArray $ mapMaybe (consider bit i) (rows m)
  filterMatrix matrix b (i + 1)

consider :: Int -> Int -> Array Int -> Maybe (Array Int)
consider comp ix arr = case arr !! ix of
                  Just n -> if n == comp then Just arr else Nothing
                  Nothing -> Nothing

toInteger :: Array Int -> Int
toInteger = foldl (\a -> (+) (2 * a)) 0

calculateBit :: Int -> Int -> Int
calculateBit height sum = if (height - sum) > sum then 0 else 1

flipBit :: Int -> Int
flipBit 0 = 1
flipBit 1 = 0
flipBit _ = 0

sumColumns :: Matrix Int -> Array Int
sumColumns = map sum <<< columns

linesToCharGrid ∷ Array String -> Array (Array Char)
linesToCharGrid = map toCharArray

charGridToIntGrid :: Array (Array Char) -> Maybe (Array (Array Int))
charGridToIntGrid = sequence <<< map (sequence <<< map charToInt)

charToInt :: Char -> Maybe Int
charToInt = decDigitToInt <<< codePointFromChar


