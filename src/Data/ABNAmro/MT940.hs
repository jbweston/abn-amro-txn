module Data.ABNAmro.MT940 (
      Parser
    , ParseError
    , Serialized
    , Field
    , parser
    , parse
    , read'
    , read''
    -- basic values
    , TxnSide
    , BalanceKind
    , Currency
    , Date
    , DateNoYear
    , Amount
    -- composite values
    , TransactionReference
    , AccountNumber
) where

import Control.Monad (liftM2, replicateM)

import Data.ByteString (ByteString, pack, unpack)
import Data.ByteString.UTF8 (toString)
import Data.Decimal
import Data.Ratio ((%))
import Data.Time (Day, fromGregorian)
import Data.Void (Void)
import Data.Word (Word8)

import Text.Megaparsec hiding (Token, ParseError, parse)
import Text.Megaparsec.Byte hiding (alphaNumChar)
import qualified Text.Megaparsec as MP

type Token = Word8
type Serialized = ByteString
type Parser = Parsec Void Serialized
type ParseError = ParseErrorBundle Serialized Void

read' :: Read a => Serialized -> a
read' = read . toString

read'' :: Read a => [Token] -> a
read'' = read' . pack

class Field a where
    parser :: Parser a

parse :: Field a => Serialized -> Either ParseError a
parse = MP.parse parser ""

-- Useful combinators

-- | apply the given parser from 1 to n times
upto :: Int -> Parser a -> Parser [a]
upto n p
    | n > 0 = liftM2 (:) p (go (n - 1) id)
    | otherwise = error "upto used with negative argument"
    where
        go k f =
            if n > 0 then do
                r <- optional p
                case r of
                    Nothing -> pure (f [])
                    Just  x -> go (k - 1) (f . (x:))
            else pure (f [])

-- | apply the given parser exactly n times
exactly :: Int -> Parser a -> Parser [a]
exactly n
    | n > 0 = replicateM n
    | otherwise = error "exactly used with negative argument"

-- Basic value definitions

-- TODO: use some template haskell to generate the parsers and
--       serializers for these simple datatypes where string
--       literals correspond 1-1 with type constructors

specialChar :: Parser Token
specialChar = oneOf $ unpack "\"\\$%&()*+-./;<= "

-- | The subset of special ASCII characters that are allowed in MT940 messages
alphanumericChar :: Parser Token
alphanumericChar = upperChar <|> digitChar <|> specialChar


data TxnSide
    = Credit
    | Debit
    | ReverseCredit
    | ReverseDebit
    deriving (Eq, Show)

instance Field TxnSide where
    parser = choice [
          Credit        <$ chunk "C"
        , Debit         <$ chunk "D"
        , ReverseCredit <$ chunk "RC"
        , ReverseDebit  <$ chunk "RD"
        ]


data BalanceKind
    = Final
    | Intermediate
    deriving (Eq, Show)

instance Field BalanceKind where
    parser = choice [
          Final        <$ chunk "F"
        , Intermediate <$ chunk "M"
        ]

data Currency
    = EUR
    | GBP
    | USD
    | CAD
    deriving (Eq, Show)

instance Field Currency where
    parser = choice [
          EUR <$ chunk "EUR"
        , GBP <$ chunk "GBP"
        , USD <$ chunk "USD"
        , CAD <$ chunk "CAD"
        ]


datePart :: Read a => Parser a
datePart = read'' <$> count 2 digitChar

newtype Date = Date Day deriving (Eq, Show)
instance Field Date where
    parser = Date <$> (fromGregorian <$> year <*> month <*> day)
        where
            year = fmap (2000 +) datePart
            month = datePart
            day = datePart

newtype DateNoYear = DateNoYear Day deriving (Eq, Show)
instance Field DateNoYear where
    parser = DateNoYear <$> (fromGregorian 0 <$> month <*> day)
        where
            month = datePart
            day = datePart


newtype Amount = Amount Decimal deriving (Eq, Show)
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


-- Composite values

data TransactionReference = TransactionReference Int Int deriving (Eq, Show)
instance Field TransactionReference where
    parser = do
        chunk ":20:"
        logicalFile <- read'' <$> exactly 8 digitChar
        sequenceNumber <- read'' <$> exactly 8 digitChar
        eol
        pure $ TransactionReference logicalFile sequenceNumber


newtype AccountNumber = AccountNumber ByteString deriving (Eq, Show)
instance Field AccountNumber where
    parser = do
        chunk ":25:"
        acct <- AccountNumber . pack <$> upto 35 alphanumericChar
        eol
        pure acct
