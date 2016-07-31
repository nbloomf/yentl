module Yentl.Algebra.Number.AlgReal (
  AlgReal(), guts
) where

import ToySolver.Data.AlgebraicNumber.Real

import Yentl.Algebra.Classes
import Yentl.Algebra.Number.Digits
import Yentl.Algebra.Number.Rational



{-----------}
{- AlgReal -}
{-----------}

data AlgReal = A
  { unA :: AReal
  } deriving (Eq, Ord)

guts :: AlgReal -> String
guts (A x) = show x



{-------------}
{- Instances -}
{-------------}

instance Num AlgReal where
  (A x) + (A y) = A (x+y)
  (A x) * (A y) = A (x*y)
  abs (A x) = A (abs x)
  signum (A x) = A (signum x)
  fromInteger = A . fromInteger
  negate (A x) = A (negate x)


instance Fractional AlgReal where
  recip (A x) = A (recip x)
  fromRational = A . fromRational


instance SquareRoot AlgReal where
  root2 (A x) = if x >= 0
    then Just $ A (nthRoot 2 x)
    else Nothing


instance ApproximateRational AlgReal where
  approximateWithin eps (A x) = approx x eps


instance Digits AlgReal where
  wholePart (A x) = abs $ truncate x

  decimalPart k x = decimalPart k r
    where r = approximateWithin (1/10^(k+1)) x


instance Show AlgReal where
  show = digits 3
