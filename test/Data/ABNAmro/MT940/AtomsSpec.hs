{-# options_ghc -Wno-orphans #-}

module Data.ABNAmro.MT940.AtomsSpec (spec) where

import Data.Decimal
import Data.Time (Day, fromGregorian)

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

import Data.ABNAmro.MT940.Atoms


-- Arbitrary instances for testing
-- TODO: find some way to remove this redundancy

instance Arbitrary TxnSide where
    arbitrary = genericArbitrary

instance Arbitrary BalanceKind where
    arbitrary = genericArbitrary

instance Arbitrary Currency where
    arbitrary = genericArbitrary

instance Arbitrary Day where
    arbitrary = fromGregorian <$> years <*> months <*> days
        where
            -- The format itself encodes years as 2 numbers, so implicitly
            -- it can only handle years in [2000, 2099].
            years = choose (2000, 2099)
            -- fromGregorian makes sure that we get a valid day/month combination
            months = arbitrary
            days = arbitrary

instance Arbitrary Date where
    arbitrary = genericArbitrary

instance Arbitrary DateNoYear where
    arbitrary = genericArbitrary


instance Arbitrary Decimal where
    arbitrary = Decimal <$> arbitrary <*> nonNegative
        where
            nonNegative :: Gen Integer
            nonNegative = fmap getNonNegative (arbitrary :: Gen (NonNegative Integer))

instance Arbitrary Amount where
    arbitrary = genericArbitrary
    

roundtrip :: Field a => a -> Either ParseError a
roundtrip = parse . serialize

spec :: Spec
spec = do
    describe "TxnSide" $
        it "parsing roundtrips with serializing" $
            property $ \(x :: TxnSide) -> roundtrip x `shouldParse`  x

    describe "BalanceKind" $
        it "parsing roundtrips with serializing" $
            property $ \(x :: BalanceKind) -> roundtrip x `shouldParse` x

    describe "Currency" $
        it "parsing roundtrips with serializing" $
            property $ \(x :: Currency) -> roundtrip x `shouldParse` x

    describe "Date" $
        it "parsing roundtrips with serializing" $
            property $ \(x :: Date) -> roundtrip x `shouldParse` x

    describe "DateNoYear" $
        it "parsing roundtrips with serializing" $
            property $ \(x :: DateNoYear) -> roundtrip x `shouldParse` x

    describe "Amount" $
        it "parsing roundtrips with serializing" $
            property $ \(x :: Amount) -> roundtrip x `shouldBe` Right x