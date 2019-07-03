module AddressParser where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fromFoldable) as A
import Data.Char.Unicode (isDigit, isLetter)
import Data.Either (either)
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.String.Common (trim)
import Data.String.Yarn (fromChars)
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.CodeUnits (char, oneOf, satisfy)
import Text.Parsing.StringParser.Combinators (many, many1, sepBy)

type Flat = String
type Line1 = String
type Line2 = String
type Line3 = String
type Line4 = String
type Postcode = String

type Address =
    {
        flatStreet :: String
    ,   line1 :: String
    ,   line2 :: String
    ,   line3 :: String
    ,   line4 :: String
    ,   postcode :: String
    }

digits :: Parser (List Char)
digits = many $ satisfy $ isDigit

toString :: forall t1 t9. Functor t1 => Foldable t9 => t1 (t9 Char) -> t1 String
toString = map (fromChars <<< A.fromFoldable)

word :: Parser String
word = toString <$> many1 $ satisfy $ \c -> isLetter c || isDigit c || c == '/'

addressLine :: Parser String
addressLine = toString <$> many1 $ satisfy (\c -> isLetter c || c == ' '  )

ruralAddressLine :: Parser String
ruralAddressLine = toString <$> many1 $ satisfy (\c -> isDigit c || isLetter c || c == ' '  )

poBox :: Parser String
poBox = toString <$> many1 $ satisfy (\c -> isDigit c || isLetter c || c == ' ')

postcode :: Parser String
postcode = toString <$> many1 $ satisfy (\c -> isDigit c)

skipWhitespaces :: Parser Unit
skipWhitespaces = void $ many $ oneOf [' ', '\n', '\t']

skipComma :: Parser Unit
skipComma = void $ char ','

sepByComma :: Parser (List String)
sepByComma = toString <$> (many1 $ satisfy $ \c ->  c /= ',')   `sepBy` char ','

sepBySpace :: Parser (List String)
sepBySpace = toString <$> (many1 $ satisfy $ \c ->  c /= ',')   `sepBy` char ' '

addressParserA2 :: Parser Address
addressParserA2 = do
    f      <- word
    skipWhitespaces 
    l1        <- addressLine 
    skipComma
    skipWhitespaces
    l4        <- addressLine
    skipWhitespaces 
    p        <- postcode
    pure $
        {
            flatStreet: f
        ,   line1: l1
        ,   line2: ""
        ,   line3: ""
        ,   line4: (trim l4)
        ,   postcode: p
        } 

addressParserA3 :: Parser Address
addressParserA3 = do
    flat      <- word
    skipWhitespaces 
    l1        <- addressLine 
    skipComma
    skipWhitespaces
    l2        <- try ruralAddressLine <|> try addressLine
    skipComma
    skipWhitespaces
    l4        <- addressLine
    skipWhitespaces 
    p        <- postcode
    pure $
        {
            flatStreet: flat
        ,   line1: l1
        ,   line2: l2
        ,   line3: ""
        ,   line4: (trim l4)
        ,   postcode: p
        } 

addressParserA4 :: Parser Address
addressParserA4 = do
    flat      <- word
    skipWhitespaces 
    l1        <- addressLine 
    skipComma
    skipWhitespaces
    l2        <- try ruralAddressLine <|> try addressLine 
    skipComma
    skipWhitespaces
    l3       <- try ruralAddressLine <|> try addressLine 
    skipComma
    skipWhitespaces
    l4        <- addressLine
    skipWhitespaces 
    p        <- postcode
    pure $
        {
            flatStreet: flat
        ,   line1: l1
        ,   line2: l2
        ,   line3: l3
        ,   line4: (trim l4)
        ,   postcode: p
        } 

addressPostalParserA2 :: Parser Address
addressPostalParserA2 = do
    pb      <- poBox
    skipComma
    skipWhitespaces 
    l4        <- addressLine
    skipWhitespaces 
    p        <- postcode
    pure $
        {
            flatStreet: ""
        ,   line1: pb
        ,   line2: ""
        ,   line3: ""
        ,   line4: (trim l4)
        ,   postcode: p
        }

addressPostalParserA3 :: Parser Address
addressPostalParserA3 = do
    pb      <- poBox
    skipComma
    skipWhitespaces 
    l2        <- try ruralAddressLine <|> try addressLine 
    skipComma
    skipWhitespaces
    l4        <- addressLine
    skipWhitespaces 
    p        <- postcode
    pure $
        {
            flatStreet: ""
        ,   line1: pb
        ,   line2: l2
        ,   line3: ""
        ,   line4: (trim l4)
        ,   postcode: p
        }

addressPostalParserA4 :: Parser Address
addressPostalParserA4 = do
    pb      <- poBox
    skipComma
    skipWhitespaces 
    l2        <- try ruralAddressLine <|> try addressLine 
    skipComma
    skipWhitespaces
    l3        <- try ruralAddressLine <|> try addressLine 
    skipComma
    skipWhitespaces
    l4        <- addressLine
    skipWhitespaces 
    p        <- postcode
    pure $
        {
            flatStreet: ""
        ,   line1: pb
        ,   line2: l2
        ,   line3: l3
        ,   line4: (trim l4)
        ,   postcode: p
        }

residentialParser :: Parser Address
residentialParser = 
        try addressParserA4
    <|> try addressParserA3 
    <|> try addressParserA2
    <|> try addressPostalParserA4
    <|> try addressPostalParserA3
    <|> try addressPostalParserA2

get :: String -> Address
get input = output 
    where
        result = runParser residentialParser input
        output = either (\_ -> empty) id  $ result
        empty = { flatStreet: "", line1: "", line2: "", line3: "", line4: "", postcode: "" }
        id = \x -> x