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
    , TransactionType
    -- composite values
    , TransactionReference
    , AccountNumber
    , StatementID
    , Transaction
) where

import Control.Monad (void)

import Data.ByteString (ByteString, pack, unpack, concat)
import Data.ByteString.UTF8 (toString)
import Data.Decimal
import Data.Ratio ((%))
import Data.Time (Day, fromGregorian)
import Data.Void (Void)
import Data.Word (Word8)

import Text.Megaparsec hiding (Token, ParseError, parse, between)
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

-- Basic value definitions

-- TODO: use some template haskell to generate the parsers and
--       serializers for these simple datatypes where string
--       literals correspond 1-1 with type constructors

alphabeticChar :: Parser Token
alphabeticChar = upperChar

specialChar :: Parser Token
specialChar = oneOf $ unpack "\"\\$%&()*+-./;<= "

-- | The subset of special ASCII characters that are allowed in MT940 messages
alphanumericChar :: Parser Token
alphanumericChar = upperChar <|> digitChar <|> specialChar

tag :: Serialized -> Parser ()
tag t = void . chunk $ ":" <> t <> ":"


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

data TransactionType = TransactionType Token Int deriving (Eq, Show)
instance Field TransactionType where
    parser =
        TransactionType
            <$> (head <$> count 1 alphabeticChar)
            <*> (read'' <$> count 3 digitChar)

-- Composite values

data TransactionReference = TransactionReference Int Int deriving (Eq, Show)
instance Field TransactionReference where
    parser = do
        tag "20"
        logicalFile <- read'' <$> count 8 digitChar
        sequenceNumber <- read'' <$> count 8 digitChar
        eol
        pure $ TransactionReference logicalFile sequenceNumber


newtype AccountNumber = AccountNumber ByteString deriving (Eq, Show)
instance Field AccountNumber where
    parser = do
        tag "25"
        acct <- AccountNumber . pack <$> count' 1 35 alphanumericChar
        eol
        pure acct

data StatementID = StatementID Int Int (Maybe Int) deriving (Eq, Show)
instance Field StatementID where
    parser = do
        tag "28"
        s <- count' 3 5 digitChar
        let (sn, rn) = splitAt (length s - 2) s
            statementNumber = read'' sn
            runNumber = read'' rn
        subMessageNumber <- fmap read'' <$> optional (chunk "/" *> count' 1 5 digitChar)
        eol
        pure $ StatementID statementNumber runNumber subMessageNumber


data Transaction =
    Transaction TransactionType Date TxnSide Amount ByteString (Maybe ByteString)
    deriving (Eq, Show)
instance Field Transaction where
    parser = do
        chunk ":61:"
        date <- parser :: Parser Date
        optional $ count 4 digitChar  -- Condensed date; we just ignore
        txnSide <- parser :: Parser TxnSide
        amount <- parser :: Parser Amount
        typeCode <- parser :: Parser TransactionType
        ref <- pack <$> count' 1 16 (notFollowedBy (chunk "//") *> alphanumericChar)
        let line = pack <$> many alphanumericChar <* eol
            marker = chunk ":"
        line  -- discard remainder of line (including abk reference)
        optional $ notFollowedBy marker *> line  -- remove transaction information
        info <- optional $ (:) <$> (chunk ":86:" *> line) <*> many (notFollowedBy marker *> line)
        pure $ Transaction typeCode date txnSide amount ref (Data.ByteString.concat <$> info)