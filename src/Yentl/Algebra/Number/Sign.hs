module Yentl.Algebra.Number.Sign (
  Sign(..), signOf
) where


data Sign
  = Positive | Negative | Zero
  deriving Eq


instance Show Sign where
  show Negative = "-"
  show _ = ""


signOf :: (Ord a, Num a) => a -> Sign
signOf x
  | x > 0 = Positive
  | x < 0 = Negative
  | otherwise = Zero
