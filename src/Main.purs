 module Main where

import Prelude

import AddressParser (Address(..), residentialParser)
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
        it "Parse \"123 street, city 1234\"" do
            let input = "123 street, city 1234"
            let result = runParser residentialParser input
            let output = either (\_ -> A2 "" "" "" "") (\x -> x)  $ result
            output `shouldEqual` A2 "123" "street" "city" "1234"
        it "Parse \"123/A Long Street, Wide City 1234\"" do
            let input = "123/A Long Street, Wide City 1234"
            let result = runParser residentialParser input
            let output = either (\_ -> A2 "" "" "" "") (\x -> x)  $ result
            output `shouldEqual` A2 "123/A" "Long Street" "Wide City" "1234"
        it "Parse \"123/A Long Street, Fanatasy Suburb, Wide City 1234\"" do
            let input = "123/A Long Street, Fanatasy Suburb, Wide City 1234"
            let result = runParser residentialParser input
            let output = either (\_ -> A3 "" "" "" "" "") (\x -> x)  $ result
            output `shouldEqual` A3 "123/A" "Long Street" "Fanatasy Suburb" "Wide City" "1234"
        it "Parse \"12/3A Long Street, Fanatasy Suburb, Area, Wide City 1234\"" do
            let input = "12/3A Long Street, Fanatasy Suburb, Area, Wide City 1234"
            let result = runParser residentialParser input
            let output = either (\_ -> A4 "" "" "" "" "" "") (\x -> x)  $ result
            output `shouldEqual` A4 "12/3A" "Long Street" "Fanatasy Suburb" "Area" "Wide City" "1234"