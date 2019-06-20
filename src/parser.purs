module AddressParser where

import Control.Alt ((<|>))
import Data.Array (fromFoldable) as A
import Data.Char.Unicode (isDigit, isLetter)
import Data.List (List)
import Data.String.Common (trim)
import Data.String.Yarn (fromChars)
import Prelude (class Show, Unit, bind, discard, map, pure, void, ($), (<$>), (<<<), (<>), (==), (||))
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.CodeUnits (char, oneOf, satisfy)
import Text.Parsing.StringParser.Combinators (many, many1)

type Flat = String
type Line1 = String
type Line2 = String
type Line3 = String
type Line4 = String
type Postcode = String

data Address =
    A2 Flat Line1 Line4 Postcode
  | A3 Flat Line1 Line2 Line4 Postcode
  | A4 Flat Line1 Line2 Line3 Line4 Postcode
  | Invalid

instance showAddress :: Show Address where
    show (A2 f l1 l4 p) = "A2[" <> f <> "|" <> l1 <> "|"  <> l4 <> "|" <> p <> "]"
    show (A3 f l1 l2 l3 p) = "A3[" <> f <> "|" <> l1 <> "|" <> l2 <> "|" <> l3 <> "|" <> p <> "]"
    show (A4 f l1 l2 l3 l4 p ) = "A4[" <> f <> "|" <> l1 <> "|" <> l2 <> "|" <> l3 <> "|" <> l4 <> "|" <> p <> "]"
    show (Invalid) = "Invalid"

instance eqAddress :: Eq Address where
    eq (A4 f l1 l2 l3 l4 p) (A4 f' l1' l2' l3' l4' p') = 
        f == f' && l1 == l1' && l2 == l2' && l3 == l3' && l4 == l4' && p == p'
    eq (A3 f l1 l2 l4 p) (A3 f' l1' l2' l4' p') = 
        f == f' && l1 == l1' && l2 == l2' && l4 == l4' && p == p'
    eq (A2 f l1 l4 p) (A2 f' l1' l4' p') = 
        f == f' && l1 == l1' && l4 == l4' && p == p'
    eq _ _ = true

digits :: Parser (List Char)
digits = many $ satisfy $ isDigit

toString = map (fromChars <<< A.fromFoldable)

word :: Parser String
word = toString <$> many1 $ satisfy $ \c -> isLetter c || isDigit c || c == '/'

-- parse a line that ends witha comma
addressLine :: Parser String
addressLine = toString<$> many1 $ satisfy (\c -> isLetter c || c == ' ' )

postcode :: Parser String
postcode = toString <$> many1 $ satisfy (\c -> isDigit c)

skipWhitespaces :: Parser Unit
skipWhitespaces = void $ many $ oneOf [' ', '\n', '\t']

skipComma :: Parser Unit
skipComma = void $ char ','

addressParserA2 :: Parser Address
addressParserA2 = do
    flat      <- word
    skipWhitespaces 
    l1        <- addressLine 
    skipComma
    skipWhitespaces
    l4        <- addressLine
    skipWhitespaces 
    p        <- postcode
    pure $ A2 
        flat 
        l1 
        (trim l4)
        p

addressParserA3 :: Parser Address
addressParserA3 = do
    flat      <- word
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
    pure $ A3
        flat 
        l1
        l2
        (trim l4)
        p 

addressParserA4 :: Parser Address
addressParserA4 = do
    flat      <- word
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
    pure $ A4
        flat 
        l1
        l2
        l3
        (trim l4)
        p 

residentialParser :: Parser Address
residentialParser = try addressParserA2 <|> try addressParserA3 <|> try addressParserA4