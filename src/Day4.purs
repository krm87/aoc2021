module Day4  where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "./day4.txt"
  log text