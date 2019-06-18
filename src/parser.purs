module AddressParser where

import Control.Alt((<|>))
import Control.Monad.Transformerless.Except (Except(..))
import Data.Array (fromFoldable) as A
import Data.Char.Unicode(isSpace, isDigit)
import Data.Functor (map)
import Data.List ((!!), head, init, last, length, tail)
import Data.Maybe (fromMaybe)
import Prelude
import Text.Parsing.Simple 
--(alphaNum, anyChar, char, choice, lookAhead, manyTill,  Parser, parse, satify sepBy, someChar, space, string, word, whiteSpace)
import Text.Parsing.Util (fromChars)

type Flat = String
type Line1 = String
type Line2 = String
type Line3 = String
type Line4 = String
type Postcode = String

data Address = Address Flat Line1 Line2 Line3 Line4 Postcode

data CustomAddress =
    A2 Flat Line1 Line4 Postcode
  | A3 Flat Line1 Line2 Line4 Postcode
  | A4 Flat Line1 Line2 Line3 Line4 Postcode
  | Unknown

instance showCustomAddress :: Show CustomAddress where
    show (A2 f l1 l4 p) = "A2[" <> f <> "|" <> l1 <> "|"  <> l4 <> "|" <> p <> "]"
    show (A3 f l1 l2 l3 p) = "A3[" <> f <> "|" <> l1 <> "|" <> l2 <> "|" <> l3 <> "|" <> p <> "]"
    show (A4 f l1 l2 l3 l4 p ) = "A4[" <> f <> "|" <> l1 <> "|" <> l2 <> "|" <> l3 <> "|" <> l4 <> "|" <> p <> "]"
    show (Unknown) = "Unknown"

mkAddress :: String -> Array (String) -> String -> CustomAddress
mkAddress f [l1, l2           ] p  = A2 f l1 l2 p
mkAddress f [l1, l2, l3       ] p  = A3 f l1 l2 l3 p
mkAddress f [l1, l2, l3, l4   ] p  = A4 f l1 l2 l3 l4 p
mkAddress _ _ _                    = Unknown

showExcept :: forall a b. Show a => Show b => Except a b -> String
showExcept (Except e) = "(Except " <> show e <> ")"

instance eqAddress :: Eq Address where
    eq (Address f l1 l2 l3 l4 p) (Address f' l1' l2' l3' l4' p') =
        f == f' && l1 == l1' && l3 == l3' && l4 == l4' && p == p'

instance showAddress :: Show Address where
    show (Address f l1 l2 l3 l4 p) = "(Address\n" 
        <> "flat:" 
        <> f 
        <> "\nline1:" 
        <> l1 
        <> "\nline2:" 
        <> l2
        <> "\nline3:" 
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

digits = someChar $ satisfy $ isDigit

isComma :: Char -> Boolean
isComma ',' = true
isComma _ = false

line :: Parser String String
line = someChar (satisfy (not <<< isComma))

lines = line `sepBy` string ", " 

comma = char ','


parseAddress input = showExcept $ parse addressParser input 

addressParser :: Parser String CustomAddress
addressParser = do
  flat      <- word -- take the flat component "123" or "123/A"
  _         <- space --skip whitespace
  ls        <- lines -- this will consume line1, line2 and line3
                               -- some cases line2 and/or line3 don't exist
 
  pure $ mkAddress flat (A.fromFoldable ls) ""
        -- Address
        --     ""
        --     (fromMaybe "" (head ls)) -- extract line1
        --     (fromMaybe "" $ tail ls >>= head) -- extract line2
        --     (fromMaybe "" $ init ls >>= last)--line3
        --     (fromMaybe "" (last ls))--line4
        --     "" --postcode
    

i4 = "123 line 1, line 2, line 3, line 4 1234"
i3 = "123 line 1, line 2, line 4 1234"
i2 = "123 line 1, line 4 1234"

