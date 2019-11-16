module Data.MT940 (
      Side (..)
    , BalanceKind (..)
    , BalanceTime (..)
    , BookingDate (..)
    , FundCode (..)
    , TransactionTypeCode (..)
    , AccountHolderReference (..)
    , BankReference (..)
    , Currency (..)
    , AccountIdentifier (..)
    , StatementIdentifier (..)
    , Balance (..)
    , Transaction (..)
    , TransactionExtraInformation (..)
    , TransactionInformation (..)
    , StatementInformation (..)
    , MT940 (..)
    , abnBIC
    , abnBankName
    ) where

import Data.ByteString (ByteString)
import Data.Time (Day)
import Data.Decimal (Decimal)


data Side
  = Credit
  | Debit
  | ReverseCredit
  | ReverseDebit
  deriving (Eq, Show)


data BalanceKind
  = Final
  | Intermediate
  deriving (Eq, Show)


data BalanceTime
  = InitialBalance
  | FinalBalance
  | ValueBalance
  | FutureValueBalance
  deriving (Eq, Show)


data Currency
  = EUR
  | GBP
  | USD
  | CAD
  deriving (Eq, Show)


-- max. 35 bytes
newtype AccountIdentifier
  = AccountIdentifier ByteString
  deriving (Eq, Show)

-- each (max.3, exactly 2, max. 5) numeric characters
newtype StatementIdentifier
  = StatementIdentifier (ByteString, Maybe ByteString, Maybe ByteString)
  deriving (Eq, Show)

data Balance
  = Balance {
      balanceKind :: BalanceKind
    , balanceSide :: Side
    , balanceDate :: Day
    , balanceCurrency :: Currency
    , balanceAmount :: Decimal
  } deriving (Eq, Show)


-- This only includes the month and day; the year is arbitrary
newtype BookingDate
  = BookingDate Day
  deriving (Eq, Show)

-- exactly 1 alpha character
data FundCode
  = FundCode
  deriving (Eq, Show)

-- exactly 1 alpha and 3 arbitrary characters
newtype TransactionTypeCode
  = TransactionTypeCode (ByteString, ByteString)
  deriving (Eq, Show)

-- max. 16 characters
-- NONREF is used if not available
newtype AccountHolderReference
  = AccountHolderReference (Maybe ByteString)
  deriving (Eq, Show)

-- max. 16 characters
newtype BankReference
  = BankReference ByteString
  deriving (Eq, Show)

-- max. 34 bytes
newtype TransactionExtraInformation
  = TransactionExtraInformation ByteString
  deriving (Eq, Show)

-- each line max. 65 bytes
-- max 6 lines
newtype TransactionInformation
  = TransactionInformation [ByteString]
  deriving (Eq, Show)


newtype StatementInformation
  = StatementInformation [ByteString]
  deriving (Eq, Show)


data Transaction = Transaction {
      transactionValueDate :: Day
    , transactionBookingDate :: BookingDate
    , transactionSide :: Side
    , transactionFundCode :: Maybe FundCode
    , transactionAmount :: Decimal
    , transactionTypeCode :: TransactionTypeCode
    , transactionAccountHolderReference :: AccountHolderReference
    , transactionBankReference :: Maybe BankReference
    , transactionExtraInformation :: Maybe TransactionExtraInformation
    , transactionInformation :: Maybe TransactionInformation
  }
  deriving (Eq, Show)



data MT940 = MT940 {
    accountIdentifier :: AccountIdentifier
  , statementIdentifier :: StatementIdentifier
  , initialBalance :: Balance
  , transactions :: [Transaction]
  , finalBalance :: Balance
  , valueBalance :: Maybe Balance
  , futureValueBalance :: Maybe Balance
  , statementInformation :: Maybe StatementInformation
  } deriving (Eq, Show)


abnBIC :: ByteString
abnBIC = "ABNANL2A"

abnBankName :: ByteString
abnBankName = "ABN AMRO BANK NV"