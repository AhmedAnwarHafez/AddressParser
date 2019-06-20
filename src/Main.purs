 module Main where

import Prelude

import AddressParser (Address(..), addressParser)
import Data.Either (either)
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Text.Parsing.StringParser (runParser)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Address Parser" do
    describe "Valid Address Formats" do
      it "123 street, city 1234" do
        let input = "123 street, city 1234"
        let result = runParser addressParser input
        let output = either (\_ -> A2 "" "" "" "") (\x -> x)  $ result
        output `shouldEqual` A2 "123" "street" "city" "1234"
