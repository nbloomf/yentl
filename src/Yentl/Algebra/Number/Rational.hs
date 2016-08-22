{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Yentl.Algebra.Number.Rational (
  sqrtWithin, ApproximateRational, approximateWithin, arctan2, sinRatD, piRat, cosRatD
) where

import Yentl.Algebra.Classes
import Yentl.Algebra.Number.Integer

instance RealField Rational

-- use heron's method to approximate the square
-- root of a positive rational
sqrtWithin :: Rational -> Rational -> Maybe Rational
sqrtWithin eps r =
  if r < 0
    then Nothing
    else Just $ fst $ head $ dropWhile tooBad $ approx
  where
    approx = iterate heron (r,r)

    heron (x,e) =
      ( (x + (r/x)) / 2
      , (e^2) / (2*(1+e))
      )

    tooBad (_,err) = err >= eps

instance SquareRoot Rational where
  root2 = sqrtWithin (1/10^6)


class ApproximateRational t where
  approximateWithin :: Rational -> t -> Rational

instance ApproximateRational Rational where
  approximateWithin _ x = x

arctan2 :: (Rational, Rational) -> Rational
arctan2 (x,y) =
  let x' = (fromRational x) :: Double in
  let y' = (fromRational y) :: Double in
  toRational $ (atan2 y' x')*180/3.14159265



-- sine and cosine of rational degree argument
sinRatD, cosRatD :: (Fractional a, Ord a) => a -> a
sinRatD t
  | t < 0     = sinRatD (t + 360)
  | t >= 360  = sinRatD (t - 360)
  | t >= 180  = negate $ sinRatD (t - 180)
  | t > 90    = sinRatD (180 - t)
  | otherwise = sum [(-1)^k * (piRat*t/180)^(2*k+1) / (fromIntegral $ factorial $ 2*k+1) | k <- [0..6]]

cosRatD t = sinRatD (90 - t)


piRat :: (Fractional a) => a
piRat = fromRational $
          3141592653589793238462643383279 /
          1000000000000000000000000000000
