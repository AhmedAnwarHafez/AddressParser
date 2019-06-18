module Main where

import AddressParser (Address(..), addressParser)
import Control.Monad.Transformerless.Except (runExcept)
import Data.Either (either)
import Data.Tuple (fst)
import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Console (log)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Text.Parsing.Simple

main :: Effect Unit
main = run [consoleReporter] do
  describe "Address Parser" do
    --describe "Valid Address Formats" do
      -- it "should be true" do
      --   let input = "123 street, city 1234"
      --   let result = runParser addressParser input
      --   let output = either (\_ -> Address "" "" "" "" "" "") (\x -> x)  $ runExcept  $ fst result
      --   output `shouldEqual` Address "123" "street" "" "" "city" "1234"
    describe "parser combinators" do    
      it "bracket combinator should return A" do
        let input = "(A)"
        let parser = (bracket  (char '(')  (char ')') (anyChar ))
        let result = runParser parser input
        let output = either (\_ -> 'N') (\x -> x)  $ runExcept  $ fst result
        output `shouldEqual` 'A'

      it "option combinator should consume \"A\" when given \"AZ\" " do
        let input = "AZ"
        let parser = ('B' `option` char 'A')
        let result = runParser parser input
        let output = either (\_ -> 'N') (\x -> x)  $ runExcept  $ fst result
        output `shouldEqual` 'A'
      it "option combinator should consume \"B\" when given \"BZ\" " do
        let input = "BZ"
        let parser = ('B' `option` char 'A')
        let result = runParser parser input
        let output = either (\_ -> 'N') (\x -> x)  $ runExcept  $ fst result
        output `shouldEqual` 'B'
      it "option combinator should consume \"street\" when given \" street\" " do
        let input = " street"
        let parser = (optional $ whiteSpace ) *> word
        let result = runParser parser input
        let output = either (\_ -> "") (\x -> x)  $ runExcept  $ fst result
        output `shouldEqual` "street"

