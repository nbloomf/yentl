{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Yentl.Algebra.Number.Digits (
  Digits, wholePart, decimalPart, digits
) where

import Data.Ratio

import Yentl.Algebra.Number.Sign



class Digits a where
  -- abs val of integer part
  wholePart :: a -> Integer

  -- digits after radix point with specified precision
  decimalPart :: Int -> a -> [Integer]

  -- string representation with specified precision
  digits :: (Ord a, Num a) => Int -> a -> String
  digits 0 x = show $ wholePart x
  digits k x = concat
    [ show $ signOf x
    , show $ wholePart x
    , "."
    , concatMap show $ decimalPart k x
    ]


instance Digits Rational where
  wholePart x = whole (abs x)
    where
      whole t =
        let
          a = numerator t
          b = denominator t
        in a `quot` b

  decimalPart k x = take k $ tail $ digits (abs x)
    where
      digits t =
        let
          a = numerator t
          b = denominator t
          q = (a `quot` b)
          m = (10 * (a`mod`b)) % b
        in q : digits m

