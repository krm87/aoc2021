module Day4  where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Data.String (Pattern(..), codePointFromChar, split)
import Matrix
import Data.Array
import Data.Maybe
import Data.Int
import Data.Traversable
import Utils

data BingoNumber = Unmarked Int | Marked Int
type BingoCard = Matrix BingoNumber

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "./day4_testinput.txt"
  let lines = readLines text
  let bingoInput = toIntArray <<< split (Pattern ",") <<< fromMaybe "" $ lines !! 0

  log $ show $ readLines text



