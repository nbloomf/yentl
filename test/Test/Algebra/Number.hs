module Test.Algebra.Number () where

import Test.QuickCheck

import Yentl.Algebra.Number.ConReal
import Yentl.Algebra.Number.AlgReal


{-----------------------}
{- Arbitrary Instances -}
{-----------------------}

instance Arbitrary ConReal where
  arbitrary = do
    h <- arbitrary
    NonZero k <- arbitrary
    return $ (fromInteger h) / (fromInteger k)


instance Arbitrary AlgReal where
  arbitrary = do
    h <- arbitrary
    NonZero k <- arbitrary
    return $ (fromInteger h) / (fromInteger k)
