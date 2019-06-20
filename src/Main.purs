 module Main where


import AddressParser (residentialParser)
import Data.Either (either)
import Effect (Effect)
import Prelude
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
            let output = either (\_ -> empty) id  $ result
            output `shouldEqual` { flatStreet: "123", line1: "street", line2: "", line3: "", line4: "city", postcode: "1234" }

        it "Parse \"123/A Long Street, Wide City 1234\"" do
            let input = "123/A Long Street, Wide City 1234"
            let result = runParser residentialParser input
            let output = either (\_ -> empty) id  $ result
            output `shouldEqual` { flatStreet: "123/A", line1: "Long Street", line2: "", line3: "", line4: "Wide City", postcode: "1234" }        

        it "Parse \"123/A Long Street, Fanatasy Suburb, Wide City 1234\"" do
            let input = "123/A Long Street, Fantasy Suburb, Wide City 1234"
            let result = runParser residentialParser input
            let output = either (\_ -> empty) id  $ result
            output `shouldEqual` { flatStreet: "123/A", line1: "Long Street", line2: "Fantasy Suburb", line3: "", line4: "Wide City", postcode: "1234" }        

        it "Parse \"12/3A Long Street, Fantasy Suburb, Area, Wide City 1234\"" do
            let input = "12/3A Long Street, Fantasy Suburb, Area, Wide City 1234"
            let result = runParser residentialParser input
            let output = either (\_ -> empty) id  $ result
            output `shouldEqual` { flatStreet: "12/3A", line1: "Long Street", line2: "Fantasy Suburb", line3: "Area", line4: "Wide City", postcode: "1234" }        

    where 
        empty = { flatStreet: "", line1: "", line2: "", line3: "", line4: "", postcode: "" }
        id = \x -> x