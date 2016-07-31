module Yentl.Algebra.Number.Integer (
  binomial, factorial
) where

binomial :: Integer -> Integer -> Integer
binomial n k
  | n <  0 || k <  0 || n < k = 0
  | n == 0 || k == n = 1
  | k == 2 = quot (n*(n-1)) 2
  | k == 3 = quot (n*(n-1)*(n-2)) 6
  | otherwise = quot (product [(k+1)..n]) (product [2..n-k])

factorial :: (Integral a, Num b) => a -> b
factorial n
  | n < 0 = error "factorial"
  | otherwise = fromIntegral $ product [1..n]
