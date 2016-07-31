module Yentl.Algebra.Number.Ord (
  monotone3, onSameSideOf1
) where


monotone3 :: (Ord a) => (a,a,a) -> Bool
monotone3 (a,b,c) = or
  [ a <= b && b <= c
  , c <= b && b <= a
  ]

onSameSideOf1 :: (Ord a) => (a,a) -> a -> Bool
onSameSideOf1 (a,b) m = or
  [ monotone3 (m,a,b)
  , monotone3 (m,b,a)
  ]
