module Main where

import AddressParser (parseAddress)
import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ parseAddress example
  where
    example = "123/A line1, line2, line3, line3 1234"
