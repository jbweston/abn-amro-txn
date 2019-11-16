{-# LANGUAGE OverloadedStrings #-}
module Parser.MT940 where

import Data.ByteString (ByteString, pack)
import Data.ByteString.UTF8 (toString)
import GHC.Word (Word8)

import Data.Void (Void)
import Data.Functor (($>))
import Data.Decimal
import Data.Ratio ((%))
import Data.Time (fromGregorian, Day)
import Text.Megaparsec
import Text.Megaparsec.Byte
import Data.MT940

read' :: Read a => [Word8] -> a
read' = read . toString . pack

encode :: MT940 -> ByteString
encode = undefined


decode :: ByteString -> Maybe MT940
decode = parseMaybe mt940


type Parser = Parsec Void ByteString


mt940 :: Parser MT940
mt940 = do
  envelope
  transactionReference
  MT940
    <$> Parser.MT940.accountIdentifier
    <*> Parser.MT940.statementIdentifier
    <*> balance InitialBalance
    <*> many transaction
    <*> balance FinalBalance
    <*> (optional . try $ balance ValueBalance)
    <*> (optional . try $ balance FutureValueBalance)
    <*> (optional . try $ Parser.MT940.statementInformation)


side :: Parser Side
side = choice [
    ReverseCredit <$ chunk "RC"
  , ReverseDebit <$ chunk "RD"
  , Debit <$ chunk "D"
  , Credit <$ chunk "C"
  ]


-- TODO: rename
balanceKindParser :: Parser BalanceKind
balanceKindParser = choice [
    Final <$ chunk "F"
  , Intermediate <$ chunk "M"
  ]


currency :: Parser Currency
currency = choice [
    EUR <$ chunk "EUR"
  , GBP <$ chunk "GBP"
  , USD <$ chunk "USD"
  , CAD <$ chunk "CAD"
  ]

fundCode :: Parser FundCode
fundCode = letterChar $> FundCode

accountHolderReferenceParser :: Parser AccountHolderReference
accountHolderReferenceParser =
  AccountHolderReference <$> (noRef <|> someRef)
  where
    someRef = Just . pack <$> some alphaNumChar
    noRef = try $ chunk "NONREF" $> Nothing


transactionTypeCodeParser :: Parser TransactionTypeCode
transactionTypeCodeParser = do
  a <- pack <$> count 1 letterChar
  b <- pack <$> count 3 alphaNumChar
  pure $ TransactionTypeCode (a, b)


datePart :: Read a => Parser a
datePart = read' <$> count 2 digitChar

date :: Parser Day
date = fromGregorian <$> year <*> month <*> day
  where
    year = fmap (2000 + ) datePart
    month = datePart
    day = datePart


bookingDate :: Parser BookingDate
bookingDate = BookingDate <$> (fromGregorian 0 <$> month <*> day)
  where
    month = datePart
    day = datePart


amount :: Parser Decimal
amount = do
  let toIntegerPart = (% 1) . read'
      toDecimalPart p = case p of
        Nothing -> 0
        Just x -> read' x % (10 ^ length x)
  integerPart <- toIntegerPart <$> some digitChar
  chunk ","
  decimalPart <- toDecimalPart <$> optional (some digitChar)
  pure . fromRational $ integerPart + decimalPart


accountIdentifier :: Parser AccountIdentifier
accountIdentifier = do
  chunk ":25:"
  -- TODO: max. 35 chars
  acct <- pack <$> some digitChar
  eol
  pure $ AccountIdentifier acct


statementIdentifier :: Parser StatementIdentifier
statementIdentifier = do
  chunk ":28" *> optional "C" *> chunk ":"
  sn' <- count 3 digitChar
  -- TODO: max 2 chars
  rn' <- optional (some digitChar)
  m' <- optional (chunk "/" >> some digitChar)
  eol
  pure $ StatementIdentifier (pack sn', pack <$> rn', pack <$> m')


balance :: BalanceTime -> Parser Balance
balance b = do
  kind' <- case b of
    InitialBalance      -> chunk ":60" *> balanceKindParser <* chunk ":"
    FinalBalance        -> chunk ":62" *> balanceKindParser <* chunk ":"
    ValueBalance        -> chunk ":64" $> Final <* chunk ":"
    FutureValueBalance  -> chunk ":65" $> Final <* chunk ":"
  side' <- side
  date' <- date
  currency' <- currency
  amount' <- amount
  eol
  pure $ Balance kind' side' date' currency' amount'


transaction :: Parser Transaction
transaction = do
  let marker = chunk ":86:"
  chunk ":61:"
  valueDate' <- date
  bookingDate' <- bookingDate
  side' <- side
  fundCode' <- optional fundCode
  amount' <- amount
  transactionTypeCode' <- transactionTypeCodeParser
  accountHolderReference' <- accountHolderReferenceParser
  eol
  txnExtraInfo <- optional $ TransactionExtraInformation <$> (eol *>notFollowedBy marker *> line)
  txnInfo <- optional transactionInformationParser
  pure $ Transaction
    valueDate'
    bookingDate'
    side'
    fundCode'
    amount'
    transactionTypeCode'
    accountHolderReference'
    Nothing -- there is never a bank reference
    txnExtraInfo
    txnInfo


line :: Parser ByteString
line = pack <$> some printChar <* eol


statementOrTransactionInformation :: Parser [ByteString]
statementOrTransactionInformation = do
  let marker = chunk ":"
  first <- chunk ":86:" *> line
  rest <- many (notFollowedBy marker *> line)
  pure $ (first : rest)


statementInformation :: Parser StatementInformation
statementInformation = StatementInformation <$> statementOrTransactionInformation


transactionInformationParser :: Parser TransactionInformation
transactionInformationParser = TransactionInformation <$> statementOrTransactionInformation


envelope :: Parser ()
envelope
  =  chunk abnBIC *> eol
  *> chunk "940"  *> eol
  *> chunk abnBIC *> eol
  $> ()

transactionReference :: Parser ()
transactionReference = chunk ":20:" *> chunk abnBankName *> eol $> ()
