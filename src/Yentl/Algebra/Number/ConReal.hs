module Yentl.Algebra.Number.ConReal (
  ConReal()
) where

import Data.Real.Constructible

import Yentl.Algebra.Classes
import Yentl.Algebra.Number.Digits
import Yentl.Algebra.Number.Sign
import Yentl.Algebra.Number.Rational



{-----------}
{- ConReal -}
{-----------}

data ConReal = C
  { unC :: Construct
  } deriving (Eq, Ord)





{-------------}
{- Instances -}
{-------------}

instance Num ConReal where
  (C x) + (C y) = C (x+y)
  (C x) * (C y) = C (x*y)
  abs (C x) = C (abs x)
  signum (C x) = C (signum x)
  fromInteger = C . fromInteger
  negate (C x) = C (negate x)


instance Fractional ConReal where
  recip (C x) = C (recip x)
  fromRational = C . fromRational


instance RealField ConReal


instance SquareRoot ConReal where
  root2 (C x) = if x >= 0
    then Just $ C (sqrt x)
    else Nothing


instance ApproximateRational ConReal where
  approximateWithin eps (C x) =
    let
      (n,f) = properFraction (abs x)
      k = head $ dropWhile (\i -> 1/10^i >= eps) [(1::Int)..]
      p = (fromIntegral $ floor $ f*10^k) / 10^k
      y = (fromIntegral n) + p
    in
      case signOf x of
        Positive -> y
        Zero     -> 0
        Negative -> -y


instance Digits ConReal where
  wholePart (C x) = floor $ abs x

  decimalPart k x = decimalPart k r
    where r = approximateWithin (1/10^(k+1)) x


instance Show ConReal where
  show x = if x == 0
    then "0"
    else  digits 3 x
