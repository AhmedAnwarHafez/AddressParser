module AddressParser where

import Control.Alt ((<|>))
import Data.Array (fromFoldable) as A
import Data.Char.Unicode (isDigit, isLetter)
import Data.List (List)
import Data.String.Common (trim)
import Data.String.Yarn (fromChars, lines)
import Prelude (class Show, class Eq, Unit, bind, discard, map, pure, void, ($), (<$>), (<<<), (<>), (==), (||), (&&), (/=))
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.CodeUnits (char, oneOf, satisfy)
import Text.Parsing.StringParser.Combinators (many, many1, sepBy)

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

data ResultAddress = ResultAddress {
        flatStreet :: String
    ,   line1 :: String
    ,   line2 :: String
    ,   line3 :: String
    ,   line4 :: String
    ,   postcode :: String
  }

instance showAddress :: Show Address where
    show (A2 f l1 l4 p) = "A2 [" <> f <> "|" <> l1 <> "|"  <> l4 <> "|" <> p <> "]"
    show (A3 f l1 l2 l3 p) = "A3 [" <> f <> "|" <> l1 <> "|" <> l2 <> "|" <> l3 <> "|" <> p <> "]"
    show (A4 f l1 l2 l3 l4 p ) = "A4 [" <> f <> "|" <> l1 <> "|" <> l2 <> "|" <> l3 <> "|" <> l4 <> "|" <> p <> "]"
    show (Invalid) = "Invalid"

instance showResultAddress :: Show ResultAddress where
    show (ResultAddress { flatStreet: f, line1:l1, line2:l2, line3:l3, line4:l4, postcode:p }) =
        "ResultAddress [" <> f <> "|" <> l1 <> "|" <> l2 <> "|" <> l3 <> "|" <> l4 <> "|" <> p <> "]"

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


addressPostalParserA2 :: Parser Address
addressPostalParserA2 = do
    l1        <- poBox 
    skipComma
    skipWhitespaces
    l4        <- addressLine
    skipWhitespaces 
    p        <- postcode
    pure $ A2 
        "" 
        l1 
        (trim l4)
        p

addressPostalParserA3 :: Parser Address
addressPostalParserA3 = do
    l1        <- poBox 
    skipComma
    skipWhitespaces
    l2        <- addressLine 
    skipComma
    skipWhitespaces
    l4        <- addressLine
    skipWhitespaces 
    p        <- postcode
    pure $ A3
        "" 
        l1 
        l2
        (trim l4)
        p

addressPostalParserA4 :: Parser Address
addressPostalParserA4 = do
    l1        <- poBox 
    skipComma
    skipWhitespaces
    l2        <- addressLine 
    skipComma
    skipWhitespaces
    l3        <- addressLine 
    skipComma
    skipWhitespaces
    l4        <- addressLine
    skipWhitespaces 
    p        <- postcode
    pure $ A4
        "" 
        l1 
        l2
        l3
        (trim l4)
        p

residentialParser :: Parser Address
residentialParser = 
        try addressParserA2
    <|> try addressParserA3 
    <|> try addressParserA4
    <|> try addressPostalParserA2
    <|> try addressPostalParserA3
    <|> try addressPostalParserA4



mkResultAddress :: Address -> ResultAddress
mkResultAddress (A2 f l1 l4 p) = 
    ResultAddress { flatStreet: f, line1: l1, line2: "", line3: "", line4: l4, postcode: p }
mkResultAddress (A3 f l1 l2 l4 p) = 
    ResultAddress { flatStreet: f, line1: l1, line2: l2, line3: "", line4: l4, postcode: p }
mkResultAddress (A4 f l1 l2 l3 l4 p) = 
    ResultAddress { flatStreet: f, line1: l1, line2: l2, line3: l3, line4: l4, postcode: p }
mkResultAddress (Invalid) = 
    ResultAddress { flatStreet: "", line1: "", line2: "", line3: "", line4: "", postcode: "" }

i4 :: String
i4 = "123 line, line, line, line 1234"

i3 :: String
i3 = "123 line, line, line 1234"

i2 :: String
i2 = "123 line, line  1234"