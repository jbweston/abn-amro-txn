{-# options_ghc -Wno-orphans #-}

module Data.ABNAmro.MT940Spec (spec) where

import Control.Applicative (liftA2)

import Data.ByteString (ByteString, pack, unpack)
import Data.ByteString.UTF8 (fromString)
import Data.Foldable
import Data.Time (Day, showGregorian)
import Data.Word

import qualified Text.Megaparsec as MP

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.Time ()

import Data.ABNAmro.MT940.Atoms


-- Useful instances for combining Gens

instance Semigroup a => Semigroup (Gen a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Gen a) where
    mempty = pure mempty

-- Character sets

nonZeroDigitChars :: Gen Word8
nonZeroDigitChars = elements $ unpack "123456789"

digitChars :: Gen Word8
digitChars = elements $ unpack "0123456789"

alphabeticChars :: Gen Word8
alphabeticChars = elements $ unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

specialChars :: Gen Word8
specialChars = elements $ unpack "\"\\$%&()*+-./;<= "

eol :: Gen ByteString
eol = elements ["\n", "\r\n"]

alphanumericChars :: Gen Word8
alphanumericChars = frequency [
      (75, alphabeticChars)
    , (20, digitChars)
    , (5, specialChars)
    ]

-- Basic value definitions

value :: Gen Word8-> Int -> Int -> Gen ByteString
value chars minSize maxSize = pack <$> (size >>= flip vectorOf chars)
    where size = choose (minSize, maxSize)

digits :: Int -> Gen ByteString
digits = value digitChars 1

digits' :: Int -> Gen ByteString
digits' n = value digitChars n n

nonZeroDigits' :: Int -> Gen ByteString
nonZeroDigits' n = value nonZeroDigitChars n n

alphabetic :: Int -> Gen ByteString
alphabetic = value alphabeticChars 1

alphabetic' :: Int -> Gen ByteString
alphabetic' n = value alphabeticChars n n

alphanumeric :: Int -> Gen ByteString
alphanumeric = value alphanumericChars 1

alphanumeric' :: Int -> Gen ByteString
alphanumeric' n = value alphanumericChars n n

-- Combinators

optional :: Gen ByteString -> Gen ByteString
optional x = oneof [x, pure ""]

between :: (Int, Int) -> Gen ByteString -> Gen ByteString
between c g = do
    n <- choose c
    fold $ replicate n g

-- Common fields

tag :: ByteString -> Gen ByteString
tag x = pure $ ":" <> x <> ":"

transactionSide :: Gen ByteString
transactionSide = elements ["C", "D"]

extendedTransactionSide :: Gen ByteString
extendedTransactionSide = elements ["C", "D", "RC", "RD"]

fullDate :: Gen ByteString
fullDate = transform <$> (arbitrary :: Gen Day)
    where
        transform = fromString . drop 2 . filter (/= '-') . showGregorian 

condensedDate :: Gen ByteString
condensedDate = transform <$> (arbitrary :: Gen Day)
    where
        transform = fromString . drop 4 . filter (/= '-') . showGregorian

currency :: Gen ByteString
currency = elements [
      "EUR"
    , "USD"
    , "CAD"
    , "GBP"
    -- TODO: add the rest of the ISO currnct codes
    ]

amount :: Gen ByteString
amount = do
    let maxWidth = 15
        maxDigits = maxWidth - 1  -- because we *must* have a decimal point
        maxDecimalPlaces = 2
        decimalPoint = pure ","
        integerPart w = nonZeroDigits' 1 <> digits' (w - 1)
        decimalPart w
            | w == 0 = pure ""
            | otherwise = digits' (w - 1) <> nonZeroDigits' 1
    dw <- choose (0, maxDecimalPlaces)
    mw <- choose (1, maxDigits - dw)
    integerPart mw <> decimalPoint <> decimalPart dw


-- Fields

transactionReferenceNumber :: Gen ByteString
transactionReferenceNumber =
    tag "20" <> alphanumeric 16 <> eol

relatedReference :: Gen ByteString
relatedReference =
    tag "21" <> alphanumeric 16 <> eol

accountNumber :: Gen ByteString
accountNumber =
    tag "25" <> alphanumeric 35 <> eol

accountStatement :: Gen ByteString
accountStatement =
       oneof  [tag "28", tag "28C"]
    <> digits 5
    <> optional (digits 5)
    <> eol

initialBookBalance :: Gen ByteString
initialBookBalance =
       oneof [tag "60F", tag "60M"]
    <> transactionSide
    <> fullDate
    <> currency
    <> amount
    <> eol

accountStatementTransaction :: Gen ByteString
accountStatementTransaction =
       tag "61"
    <> fullDate
    <> condensedDate
    <> extendedTransactionSide
    <> optional (alphabetic' 1)
    <> amount
    <> alphanumeric' 4
    <> alphanumeric 16
    <> optional (alphanumeric 16)
    <> optional (eol <> alphanumeric 34)
    <> eol

transactionInformation :: Gen ByteString
transactionInformation =
       tag "86"
    <> between (1, 6) (alphanumeric 65 <> eol)

finalBookBalance :: Gen ByteString
finalBookBalance =
       oneof [tag "62F", tag "62M"]
    <> transactionSide
    <> fullDate
    <> currency
    <> amount
    <> eol

valueBalance :: Gen ByteString
valueBalance =
       tag "64"
    <> transactionSide
    <> fullDate
    <> currency
    <> amount
    <> eol

futureValueBalance :: Gen ByteString
futureValueBalance =
       tag "65"
    <> transactionSide
    <> fullDate
    <> currency
    <> amount
    <> eol

messageInformation :: Gen ByteString
messageInformation =
       tag "86"
    <> between (1, 6) (alphanumeric 65 <> eol)

-- full message

-- we only support envelope type 2 for now

envelopeHeader :: Gen ByteString
envelopeHeader =
       pure "ABNANL2A" <> eol
    <> pure "940" <> eol
    <> pure "ABNANL2A" <> eol

envelopeTrailer :: Gen ByteString
envelopeTrailer =
    pure "-" <> eol

mt940Message :: Gen ByteString
mt940Message =
    envelopeHeader
        <> transactionReferenceNumber
        <> optional relatedReference
        <> accountNumber
        <> accountStatement
        <> initialBookBalance
        <> between (0, 100) (
               accountStatementTransaction
            <> transactionInformation
        )
        <> finalBookBalance
        <> optional valueBalance
        <> between (0, 100) futureValueBalance
        <> messageInformation
    <> envelopeTrailer

-- Specs

newtype Any a = Any (Gen ByteString)

canParse :: forall a. (Show a, Field a) => Any a -> SpecWith (Arg Property)
canParse (Any gen) =
    let isParsedBy p = shouldSucceedOn (MP.parse p "")
    in it "can parse anything" $ forAll gen $ isParsedBy (parser :: Parser a)

spec :: Spec
spec = do
    -- Common fields
    describe "TxnSide" $
        canParse (Any transactionSide :: Any TxnSide)
    describe "Currency" $
        canParse (Any currency :: Any Currency)
    describe "Date" $
        canParse (Any fullDate :: Any Date)
    describe "DateNoYear" $
        canParse (Any condensedDate :: Any DateNoYear)
    describe "Amount" $
        canParse (Any amount :: Any Amount)