module Data.ABNAmro.MT940 (
      Parser
    , ParseError
    , Serialized
    , Field
    , parser
    , serializer
    , parse
    , serialize
    , serialized
    , read'
    , read''
    , TxnSide
    , BalanceKind
    , Currency
    , Date
    , DateNoYear
    , Amount
) where

import Control.Arrow ((>>>))

import Data.ByteString (ByteString, pack)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Coerce (coerce)
import Data.Decimal
import Data.Ratio ((%))
import Data.Time (Day, fromGregorian, showGregorian)
import Data.Void (Void)
import Data.Word (Word8)

import GHC.Generics

import Text.Megaparsec hiding (Token, ParseError, parse)
import Text.Megaparsec.Byte
import qualified Text.Megaparsec as MP

type Token = Word8
type Serialized = ByteString
type Parser = Parsec Void Serialized
type Serializer a = a -> Serialized
type ParseError = ParseErrorBundle Serialized Void

serialized :: String -> Serialized
serialized = fromString

read' :: Read a => Serialized -> a
read' = read . toString

read'' :: Read a => [Token] -> a
read'' = read' . pack

class Field a where
    serializer :: Serializer a
    parser :: Parser a

parse :: Field a => Serialized -> Either ParseError a
parse = MP.parse parser ""

serialize :: Field a => a -> Serialized
serialize = serializer


-- Basic value definitions

-- TODO: use some template haskell to generate the parsers and
--       serializers for these simple datatypes where string
--       literals correspond 1-1 with type constructors

data TxnSide
    = Credit
    | Debit
    | ReverseCredit
    | ReverseDebit
    deriving (Eq, Generic, Show)

instance Field TxnSide where
    parser = choice [
          Credit        <$ chunk "C"
        , Debit         <$ chunk "D"
        , ReverseCredit <$ chunk "RC"
        , ReverseDebit  <$ chunk "RD"
        ]
    serializer t = case t of
        Credit        -> "C"
        Debit         -> "D"
        ReverseCredit -> "RC"
        ReverseDebit  -> "RD"


data BalanceKind
    = Final
    | Intermediate
    deriving (Eq, Generic, Show)

instance Field BalanceKind where
    parser = choice [
          Final        <$ chunk "F"
        , Intermediate <$ chunk "M"
        ]
    serializer k = case k of
        Final        -> "F"
        Intermediate -> "M"


data Currency
    = EUR
    | GBP
    | USD
    | CAD
    deriving (Eq, Generic, Show)

instance Field Currency where
    parser = choice [
          EUR <$ chunk "EUR"
        , GBP <$ chunk "GBP"
        , USD <$ chunk "USD"
        , CAD <$ chunk "CAD"
        ]
    -- The serialized form is the constructor name
    serializer = show >>> serialized


datePart :: Read a => Parser a
datePart = read'' <$> count 2 digitChar

newtype Date = Date Day deriving (Eq, Generic, Show)
instance Field Date where
    parser = Date <$> (fromGregorian <$> year <*> month <*> day)
        where
            year = fmap (2000 +) datePart
            month = datePart
            day = datePart
    serializer = coerce >>> showGregorian >>> filter (/= '-') >>> drop 2 >>> serialized

newtype DateNoYear = DateNoYear Day deriving (Eq, Generic, Show)
instance Field DateNoYear where
    parser = DateNoYear <$> (fromGregorian 0 <$> month <*> day)
        where
            month = datePart
            day = datePart
    serializer = coerce >>> showGregorian >>> filter (/= '-') >>> drop 4 >>> serialized


newtype Amount = Amount Decimal deriving (Eq, Generic, Show)
instance Field Amount where
    parser = do
        let toIntegerPart = (% 1) . read''
            toDecimalPart p = case p of
                Nothing -> 0
                Just x -> read'' x % (10 ^ length x)
        integerPart <- toIntegerPart <$> some digitChar
        chunk ","
        decimalPart <- toDecimalPart <$> optional (some digitChar)
        pure . Amount . fromRational $ integerPart + decimalPart
    serializer
        = (coerce :: (Amount -> Decimal))  -- We need to give the type checker a hand
        >>> show >>> replace '.' ',' >>> ensureDecimal >>> serialized
        where
            replace x y = map (\c -> if c == x then y else c)
            ensureDecimal s
                | ',' `elem` s = s
                | otherwise    = s <> ","