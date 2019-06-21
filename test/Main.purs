 module Main where

import AddressParser (get)
import Effect (Effect)
import Prelude
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Address Parser" do
    describe "Residential addresses tests" do

        it "with 2 lines" do
            let input = "123 street, city 1234"
            get input `shouldEqual` { flatStreet: "123", line1: "street", line2: "", line3: "", line4: "city", postcode: "1234" }

        it "with 3 lines" do
            let input = "123/A Long Street, Wide City 1234"
            get input `shouldEqual` { flatStreet: "123/A", line1: "Long Street", line2: "", line3: "", line4: "Wide City", postcode: "1234" }        

        it "with 4 lines" do
            let input = "123/A Long Street, Fantasy Suburb, Wide City 1234"
            get input `shouldEqual` { flatStreet: "123/A", line1: "Long Street", line2: "Fantasy Suburb", line3: "", line4: "Wide City", postcode: "1234" }        

        it "with 4 lines and sepecial characters in flatStreet" do
            let input = "12/3A Long Street, Fantasy Suburb, Area, Wide City 1234"
            get input `shouldEqual` { flatStreet: "12/3A", line1: "Long Street", line2: "Fantasy Suburb", line3: "Area", line4: "Wide City", postcode: "1234" }        

    describe "Postal addresses tests" do
       
        it "with 2 lines" do
            let input = "Private Bag 111, Kaikohe 4444"
            get input `shouldEqual` { flatStreet: "", line1: "Private Bag 111", line2: "", line3: "", line4: "Kaikohe", postcode: "4444" }        

        it "with 3 lines" do
            let input = "PO Box 111, Mail Centre, Hamilton 3240"
            get input `shouldEqual` { flatStreet: "", line1: "PO Box 111", line2: "Mail Centre", line3: "", line4: "Hamilton", postcode: "3240" }        

        it "with 4 lines" do
            let input = "Private Bag 333, Suburb, Mail Center, Kaikohe 0440"
            get input `shouldEqual` { flatStreet: "", line1: "Private Bag 333", line2: "Suburb", line3: "Mail Center", line4: "Kaikohe", postcode: "0440" }        