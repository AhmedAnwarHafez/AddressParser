module AddressParser where

import Control.Monad.Transformerless.Except (Except(..))
import Data.Functor (map)
import Data.List ((!!), head, last)
import Data.Maybe (fromMaybe)
import Prelude
import Text.Parsing.Simple (alphaNum, char, choice, lookAhead, manyTill,  Parser, parse, sepBy, space, string, word)
import Text.Parsing.Util (fromChars)

type Flat = String
type Line1 = String
type Line2 = String
type Line3 = String
type Line4 = String
type Postcode = String

data Address = Address Flat Line1 Line2 Line3 Line4 Postcode

showExcept :: forall a b. Show a => Show b => Except a b -> String
showExcept (Except e) = "(Except " <> show e <> ")"

instance showAddress :: Show Address where
    show (Address f l1 l2 l3 l4 p) = "(Address\n" 
        <> "flat:" 
        <> f 
        <> "\nline1:" 
        <> l1 
        <> "\nline2:" 
        <> l2
        <> "\nine3:" 
        <> l3
        <> "\nline4:" 
        <> l4 
        <> "\npostcode:" 
        <> p 
        <> "\n)"

anyDigit :: Parser String Char
anyDigit = choice $ map (char) 
    [ '0'
    , '1'
    , '2'
    , '3'
    , '4'
    , '5'
    , '6'
    , '7' 
    , '8'
    , '9'
    ]

myWord = manyTill alphaNum (lookAhead $ char ',')

lineOneTwoThree = myWord `sepBy` string ", " 

comma = char ','


parseAddress input = showExcept $ parse addressParser input 

addressParser :: Parser String Address
addressParser = do
  flat      <- word -- take the flat component "123" or "123/A"
  _         <- space --skip whitespace
  lines     <- lineOneTwoThree -- this will consume line1, line2 and line3
                               -- some cases line2 and/or line3 don't exist
  _         <- comma -- skip comma
  _         <- space --skip whitespace
  line4     <- word  -- take the last line   
  _         <- space --skip whitespace
  postcode  <- word -- take the postcode
  pure $ Address
            flat
            (fromMaybe "NF" (head $ map fromChars lines)) -- extract line1
            (fromMaybe "" ((map fromChars lines) !! 1 )) -- extract line2, if it doesn't exist then return an empty string
            (fromMaybe "" ((map fromChars lines) !! 2 )) -- same as above
            line4
            postcode
    

--input = "299/A Long Street, Wide County, La La Land 1234"
--input' = "line1, line2, line3, line4"

   

