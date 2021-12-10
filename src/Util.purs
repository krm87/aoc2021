module Utils where

import Data.Array
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Prelude
import Data.String
import Data.Traversable
import Data.Int

tailOrEmpty :: forall a. Array a -> Array a
tailOrEmpty xs = case tail xs of
                  Just t -> t
                  Nothing -> []

betweenSorted :: Int -> Int -> Int -> Boolean
betweenSorted x x' x'' = (between (min x' x'') (max x' x'')) x

readLines :: String -> Array String
readLines = split (Pattern "\n")

toIntArray :: Array String -> Maybe (Array Int)
toIntArray = sequence <<< map fromString