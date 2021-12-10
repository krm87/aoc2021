module Day5 where

import Data.Traversable (sequence)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Prelude

import Data.Array (concat, filter, head, (!!), (..))
import Data.HashMap as HM
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Utils (readLines, tailOrEmpty)

type Coordinate = Tuple Int Int
type Line = Tuple Coordinate Coordinate

main :: Effect Unit
main = do
    text <- readTextFile UTF8 "./src/Day5/day5.txt"
    let txtlines = readLines text
    case sequence $ map (toLine <=< parseLine) txtlines of
        Just lines -> do
            let straightLines = filter (not isDiagonal) lines
            let diagonals = filter isDiagonal lines
            let straightCoordinates = concat $ map lineToCoords straightLines
            let diagonalCoords = concat $ map diagonalToCoords diagonals
            let coordinates = straightCoordinates <> diagonalCoords
            let hashmap = coordsToHashMap coordinates HM.empty
            log $ "Coordinates with line sections: " <> show ( HM.size $ HM.filter (\v-> v>1) hashmap )
        Nothing -> log $ "Unable to parse input"
    pure unit

coordsToHashMap :: Array Coordinate -> HM.HashMap Coordinate Int -> HM.HashMap Coordinate Int
coordsToHashMap [] m = m
coordsToHashMap xs m = coordsToHashMap (tailOrEmpty xs) newmap
    where
        h = headOrDefault xs
        newmap = HM.insertWith (+) h 1 m

headOrDefault :: Array Coordinate -> Coordinate
headOrDefault xs = case head xs of
                    Just x -> x
                    Nothing -> Tuple 0 0

maxCoordinate :: Tuple Int Int -> Tuple Int Int -> Tuple Int Int
maxCoordinate (Tuple x y) (Tuple x' y') = Tuple (max x x') (max y y')

lineToCoords :: Line -> Array Coordinate
lineToCoords (Tuple (Tuple x y) (Tuple x' y')) = do
  xs <- x .. x'
  ys <- y .. y'
  pure $ (Tuple xs ys)

diagonalToCoords :: Line -> Array Coordinate
diagonalToCoords (Tuple (Tuple x y) (Tuple x' y')) = do
    xs <- x .. x'
    let ys = slope * xs + b
    pure (Tuple xs ys)
    where
        b = y - slope * x
        slope = ((y' - y) / (x' - x))

isDiagonal :: Line -> Boolean
isDiagonal (Tuple (Tuple x y) (Tuple x' y')) | y /= y' && x /= x' = true
                                             | otherwise = false

parseLine :: String -> Maybe (Tuple String String)
parseLine xs = do
    let vals = split (Pattern " ") xs
    xy1 <- vals !! 0
    xy2 <- vals !! 2
    pure (Tuple xy1 xy2)

toLine :: (Tuple String String) -> Maybe Line
toLine (Tuple xy1 xy2) = do
    xy1' <- sequence <<< map fromString $ split (Pattern ",") xy1
    xy2' <- sequence <<< map fromString $ split (Pattern ",") xy2
    x1 <- xy1' !! 0
    y1 <- xy1' !! 1
    x2 <- xy2' !! 0
    y2 <- xy2' !! 1
    pure (Tuple (Tuple x1 y1) (Tuple x2 y2))