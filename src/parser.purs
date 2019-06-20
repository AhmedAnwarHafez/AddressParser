module AddressParser where

import Control.Alt ((<|>))
import Data.Array (fromFoldable) as A
import Data.Char.Unicode (isDigit, isLetter)
import Data.List (List)
import Data.String.Common (trim)
import Data.String.Yarn (fromChars)
import Prelude
import Text.Parsing.StringParser (Parser, try)
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

toString = map (fromChars <<< A.fromFoldable)

word :: Parser String
word = toString <$> many1 $ satisfy $ \c -> isLetter c || isDigit c || c == '/'

-- parse a line that ends witha comma
addressLine :: Parser String
addressLine = toString <$> many1 $ satisfy (\c -> isLetter c || c == ' ' )

poBox :: Parser String
poBox = toString <$> many1 $ satisfy (\c -> isDigit c || isLetter c || c == ' ' )

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
    f      <- try word <|> try poBox
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
    flat      <- try word <|> try poBox
    skipWhitespaces 
    l1        <- addressLine 
    skipComma
    skipWhitespaces
    l2        <- addressLine 
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
    flat      <- try word <|> try poBox
    skipWhitespaces 
    l1        <- addressLine 
    skipComma
    skipWhitespaces
    l2        <- addressLine 
    skipComma
    skipWhitespaces
    l3       <- addressLine 
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

residentialParser :: Parser Address
residentialParser = 
        try addressParserA2
    <|> try addressParserA3 
    <|> try addressParserA4

i4 :: String
i4 = "123 line, line, line, line 1234"

i3 :: String
i3 = "123 line, line, line 1234"

i2 :: String
i2 = "123 line, line  1234"