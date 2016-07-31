{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yentl.Algebra.Data.Mat3 (
  Mat3(), fromRows3, toRows3, orientation
) where

import Yentl.Algebra.Classes
import Yentl.Algebra.Number.Sign



{- Mat3 -}

data Mat3 a = Mat3
  { b11 :: a, b12 :: a, b13 :: a
  , b21 :: a, b22 :: a, b23 :: a
  , b31 :: a, b32 :: a, b33 :: a
  } deriving Eq

fromRows3 :: ((a,a,a),(a,a,a),(a,a,a)) -> Mat3 a
fromRows3 ((a,b,c),(d,e,f),(g,h,i)) = Mat3
  { b11 = a, b12 = b, b13 = c
  , b21 = d, b22 = e, b23 = f
  , b31 = g, b32 = h, b33 = i
  }

toRows3 :: Mat3 a -> ((a,a,a),(a,a,a),(a,a,a))
toRows3 m =
  ( (b11 m, b12 m, b13 m)
  , (b21 m, b22 m, b23 m)
  , (b31 m, b32 m, b33 m)
  )



{- Show -}

instance (Show a) => Show (Mat3 a) where
  show m = concat
    [ "[[", show (b11 m), ",", show (b12 m), ",", show (b13 m), "],["
    , show (b21 m), ",", show (b22 m), ",", show (b23 m), "],["
    , show (b31 m), ",", show (b32 m), ",", show (b33 m), "]]"
    ]



{- Module -}

instance (Num a) => Module a (Mat3 a) where
  a +. b = Mat3
    { b11 = (b11 a)+(b11 b), b12 = (b12 a)+(b12 b), b13 = (b13 a)+(b13 b)
    , b21 = (b21 a)+(b21 b), b22 = (b22 a)+(b22 b), b23 = (b23 a)+(b23 b)
    , b31 = (b31 a)+(b31 b), b32 = (b32 a)+(b32 b), b33 = (b33 a)+(b33 b)
    }

  zero = Mat3
    { b11 = 0, b12 = 0, b13 = 0
    , b21 = 0, b22 = 0, b23 = 0
    , b31 = 0, b32 = 0, b33 = 0
    }

  minus a = Mat3
    { b11 = negate (b11 a), b12 = negate (b12 a), b13 = negate (b13 a)
    , b21 = negate (b21 a), b22 = negate (b22 a), b23 = negate (b23 a)
    , b31 = negate (b31 a), b32 = negate (b32 a), b33 = negate (b33 a)
    }

  x @. a = Mat3
    { b11 = x * (b11 a), b12 = x * (b12 a), b13 = x * (b13 a)
    , b21 = x * (b21 a), b22 = x * (b22 a), b23 = x * (b23 a)
    , b31 = x * (b31 a), b32 = x * (b32 a), b33 = x * (b33 a)
    }



{- SquareMatrix -}

instance (Eq a, Num a) => SquareMatrix a (Mat3 a) where
  a *. b = Mat3
    { b11 = (b11 a)*(b11 b) + (b12 a)*(b21 b) + (b13 a)*(b31 b)
    , b12 = (b11 a)*(b12 b) + (b12 a)*(b22 b) + (b13 a)*(b32 b)
    , b13 = (b11 a)*(b13 b) + (b12 a)*(b23 b) + (b13 a)*(b33 b)
    , b21 = (b21 a)*(b11 b) + (b22 a)*(b21 b) + (b23 a)*(b31 b)
    , b22 = (b21 a)*(b12 b) + (b22 a)*(b22 b) + (b23 a)*(b32 b)
    , b23 = (b21 a)*(b13 b) + (b22 a)*(b23 b) + (b23 a)*(b33 b)
    , b31 = (b31 a)*(b11 b) + (b32 a)*(b21 b) + (b33 a)*(b31 b)
    , b32 = (b31 a)*(b12 b) + (b32 a)*(b22 b) + (b33 a)*(b32 b)
    , b33 = (b31 a)*(b13 b) + (b32 a)*(b23 b) + (b33 a)*(b33 b)
    }

  one = Mat3
    { b11 = 1, b12 = 0, b13 = 0
    , b21 = 0, b22 = 1, b23 = 0
    , b31 = 0, b32 = 0, b33 = 1
    }

  det m
    =   (b11 m)*(b22 m)*(b33 m) -- > (1)
      + (b12 m)*(b23 m)*(b31 m) -- \ (3)
      + (b13 m)*(b21 m)*(b32 m) -- /
      - (b13 m)*(b22 m)*(b31 m) -- \
      - (b12 m)*(b21 m)*(b33 m) -- | (2)
      - (b11 m)*(b23 m)*(b32 m) -- /

  invert m = do
    let d = det m
    if d == 0
      then Nothing
      else undefined



orientation :: (Ord a, Num a) => (a,a) -> (a,a) -> (a,a) -> Sign
orientation (xa,ya) (xb,yb) (xc,yc) = signOf $ det $ fromRows3
  ((xa, ya, 1), (xb, yb, 1), (xc, yc, 1))
