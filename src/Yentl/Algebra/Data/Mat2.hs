{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yentl.Algebra.Data.Mat2 (
  Mat2(), fromRows2, toRows2
) where

import Yentl.Algebra.Classes


{- Mat2 -}

data Mat2 a = Mat2
  { a11 :: a, a12 :: a
  , a21 :: a, a22 :: a
  } deriving Eq

fromRows2 :: ((a,a),(a,a)) -> Mat2 a
fromRows2 ((a,b),(c,d)) = Mat2
  { a11 = a, a12 = b
  , a21 = c, a22 = d
  }

toRows2 :: Mat2 a -> ((a,a),(a,a))
toRows2 m =
  ( (a11 m, a12 m)
  , (a21 m, a22 m)
  )



{- Show -}

instance (Show a) => Show (Mat2 a) where
  show m = concat
    [ "[[", show (a11 m), ",", show (a12 m), "],["
    , show (a21 m), ",", show (a22 m), "]]"
    ]



{- Module -}

instance (Num a) => Module a (Mat2 a) where
  a +. b = Mat2
    { a11 = (a11 a)+(a11 b), a12 = (a12 a)+(a12 b)
    , a21 = (a21 a)+(a21 b), a22 = (a22 a)+(a22 b)
    }

  zero = Mat2
    { a11 = 0, a12 = 0, a21 = 0, a22 = 0 }

  minus a = Mat2
    { a11 = negate (a11 a), a12 = negate (a12 a)
    , a21 = negate (a21 a), a22 = negate (a22 a)
    }

  x @. a = Mat2
    { a11 = x * (a11 a), a12 = x * (a12 a)
    , a21 = x * (a21 a), a22 = x * (a22 a)
    }



{- SquareMatrix -}

instance (Eq a, Num a, Fractional a) => SquareMatrix a (Mat2 a) where
  a *. b = Mat2
    { a11 = (a11 a)*(a11 b) + (a12 a)*(a21 b)
    , a12 = (a11 a)*(a12 b) + (a12 a)*(a22 b)
    , a21 = (a21 a)*(a11 b) + (a22 a)*(a21 b)
    , a22 = (a21 a)*(a12 b) + (a22 a)*(a22 b)
    }

  one = Mat2
    { a11 = 1, a12 = 0
    , a21 = 0, a22 = 1
    }

  det m = (a11 m)*(a22 m) - (a12 m)*(a21 m)

  invert m = do
    let d = det m
    if d == 0
      then Nothing
      else Just $ Mat2
             { a11 = (a22 m)/d, a12 = (negate $ a12 m)/d
             , a21 = (negate $ a21 m)/d, a22 = (a11 m)/d
             }



{- MatMult -}

instance (Num a) => MatMult (Mat2 a) (a,a) where
  m %. (v1,v2) =
    ( (a11 m)*v1 + (a12 m)*v2
    , (a21 m)*v1 + (a22 m)*v2
    )
