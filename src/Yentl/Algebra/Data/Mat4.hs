{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yentl.Algebra.Data.Mat4 (
  Mat4(), fromRows4, toRows4
) where

import Yentl.Algebra.Classes



{--------}
{- Mat4 -}
{--------}

data Mat4 a = Mat4
  { c11 :: a, c12 :: a, c13 :: a, c14 :: a
  , c21 :: a, c22 :: a, c23 :: a, c24 :: a
  , c31 :: a, c32 :: a, c33 :: a, c34 :: a
  , c41 :: a, c42 :: a, c43 :: a, c44 :: a
  } deriving Eq

fromRows4 :: ((a,a,a,a),(a,a,a,a),(a,a,a,a),(a,a,a,a)) -> Mat4 a
fromRows4 ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) = Mat4
  { c11 = a, c12 = b, c13 = c, c14 = d
  , c21 = e, c22 = f, c23 = g, c24 = h
  , c31 = i, c32 = j, c33 = k, c34 = l
  , c41 = m, c42 = n, c43 = o, c44 = p
  }

toRows4 :: Mat4 a -> ((a,a,a,a),(a,a,a,a),(a,a,a,a),(a,a,a,a))
toRows4 m =
  ( (c11 m, c12 m, c13 m, c14 m)
  , (c21 m, c22 m, c23 m, c24 m)
  , (c31 m, c32 m, c33 m, c34 m)
  , (c41 m, c42 m, c43 m, c44 m)
  )



{-------------}
{- Instances -}
{-------------}

{- Show -}

instance (Show a) => Show (Mat4 a) where
  show m = concat
    [ "[[", show (c11 m), ",", show (c12 m), ",", show (c13 m), ",", show (c14 m), "],["
    , show (c21 m), ",", show (c22 m), ",", show (c23 m), ",", show (c24 m), "],["
    , show (c31 m), ",", show (c32 m), ",", show (c33 m), ",", show (c34 m), "],["
    , show (c41 m), ",", show (c42 m), ",", show (c43 m), ",", show (c44 m), "]]"
    ]



{- Module -}

instance (Num a) => Module a (Mat4 a) where
  a +. b = Mat4
    { c11 = (c11 a)+(c11 b), c12 = (c12 a)+(c12 b)
    , c13 = (c13 a)+(c13 b), c14 = (c14 a)+(c14 b)
    , c21 = (c21 a)+(c21 b), c22 = (c22 a)+(c22 b)
    , c23 = (c23 a)+(c23 b), c24 = (c24 a)+(c24 b)
    , c31 = (c31 a)+(c31 b), c32 = (c32 a)+(c32 b)
    , c33 = (c33 a)+(c33 b), c34 = (c34 a)+(c34 b)
    , c41 = (c41 a)+(c41 b), c42 = (c42 a)+(c42 b)
    , c43 = (c43 a)+(c43 b), c44 = (c44 a)+(c44 b)
    }

  zero = Mat4
    { c11 = 0, c12 = 0, c13 = 0, c14 = 0
    , c21 = 0, c22 = 0, c23 = 0, c24 = 0
    , c31 = 0, c32 = 0, c33 = 0, c34 = 0
    , c41 = 0, c42 = 0, c43 = 0, c44 = 0
    }

  minus a = Mat4
    { c11 = negate (c11 a), c12 = negate (c12 a), c13 = negate (c13 a), c14 = negate (c14 a)
    , c21 = negate (c21 a), c22 = negate (c22 a), c23 = negate (c23 a), c24 = negate (c24 a)
    , c31 = negate (c31 a), c32 = negate (c32 a), c33 = negate (c33 a), c34 = negate (c34 a)
    , c41 = negate (c41 a), c42 = negate (c42 a), c43 = negate (c43 a), c44 = negate (c44 a)
    }

  x @. a = Mat4
    { c11 = x * (c11 a), c12 = x * (c12 a), c13 = x * (c13 a), c14 = x * (c14 a)
    , c21 = x * (c21 a), c22 = x * (c22 a), c23 = x * (c23 a), c24 = x * (c24 a)
    , c31 = x * (c31 a), c32 = x * (c32 a), c33 = x * (c33 a), c34 = x * (c34 a)
    , c41 = x * (c41 a), c42 = x * (c42 a), c43 = x * (c43 a), c44 = x * (c44 a)
    }



{- SquareMatrix -}

instance (Eq a, Num a) => SquareMatrix a (Mat4 a) where
  a *. b = Mat4
    { c11 = (c11 a)*(c11 b) + (c12 a)*(c21 b) + (c13 a)*(c31 b) + (c14 a)*(c41 b)
    , c12 = (c11 a)*(c12 b) + (c12 a)*(c22 b) + (c13 a)*(c32 b) + (c14 a)*(c42 b)
    , c13 = (c11 a)*(c13 b) + (c12 a)*(c23 b) + (c13 a)*(c33 b) + (c14 a)*(c43 b)
    , c14 = (c11 a)*(c14 b) + (c12 a)*(c24 b) + (c13 a)*(c34 b) + (c14 a)*(c44 b)
    , c21 = (c21 a)*(c11 b) + (c22 a)*(c21 b) + (c23 a)*(c31 b) + (c24 a)*(c41 b)
    , c22 = (c21 a)*(c12 b) + (c22 a)*(c22 b) + (c23 a)*(c32 b) + (c24 a)*(c42 b)
    , c23 = (c21 a)*(c13 b) + (c22 a)*(c23 b) + (c23 a)*(c33 b) + (c24 a)*(c43 b)
    , c24 = (c21 a)*(c14 b) + (c22 a)*(c24 b) + (c23 a)*(c34 b) + (c24 a)*(c44 b)
    , c31 = (c31 a)*(c11 b) + (c32 a)*(c21 b) + (c33 a)*(c31 b) + (c34 a)*(c41 b)
    , c32 = (c31 a)*(c12 b) + (c32 a)*(c22 b) + (c33 a)*(c32 b) + (c34 a)*(c42 b)
    , c33 = (c31 a)*(c13 b) + (c32 a)*(c23 b) + (c33 a)*(c33 b) + (c34 a)*(c43 b)
    , c34 = (c31 a)*(c14 b) + (c32 a)*(c24 b) + (c33 a)*(c34 b) + (c34 a)*(c44 b)
    , c41 = (c41 a)*(c11 b) + (c42 a)*(c21 b) + (c43 a)*(c31 b) + (c44 a)*(c41 b)
    , c42 = (c41 a)*(c12 b) + (c42 a)*(c22 b) + (c43 a)*(c32 b) + (c44 a)*(c42 b)
    , c43 = (c41 a)*(c13 b) + (c42 a)*(c23 b) + (c43 a)*(c33 b) + (c44 a)*(c43 b)
    , c44 = (c41 a)*(c14 b) + (c42 a)*(c24 b) + (c43 a)*(c34 b) + (c44 a)*(c44 b)
    }

  one = Mat4
    { c11 = 1, c12 = 0, c13 = 0, c14 = 0
    , c21 = 0, c22 = 1, c23 = 0, c24 = 0
    , c31 = 0, c32 = 0, c33 = 1, c34 = 0
    , c41 = 0, c42 = 0, c43 = 0, c44 = 1
    }

  det m
    =   (c11 m)*(c22 m)*(c33 m)*(c44 m) -- > (1)
      + (c12 m)*(c21 m)*(c34 m)*(c43 m) -- \
      + (c13 m)*(c24 m)*(c31 m)*(c42 m) -- | (2,2)
      + (c14 m)*(c23 m)*(c32 m)*(c41 m) -- /
      + (c12 m)*(c23 m)*(c31 m)*(c44 m) -- \
      + (c13 m)*(c21 m)*(c32 m)*(c44 m) -- | (3)
      + (c12 m)*(c24 m)*(c33 m)*(c41 m) -- |
      + (c14 m)*(c21 m)*(c33 m)*(c42 m) -- |
      + (c13 m)*(c22 m)*(c34 m)*(c41 m) -- |
      + (c14 m)*(c22 m)*(c31 m)*(c43 m) -- |
      + (c11 m)*(c23 m)*(c34 m)*(c42 m) -- |
      + (c11 m)*(c24 m)*(c32 m)*(c43 m) -- /
      - (c12 m)*(c21 m)*(c33 m)*(c44 m) -- \
      - (c13 m)*(c22 m)*(c31 m)*(c44 m) -- | (2)
      - (c14 m)*(c22 m)*(c33 m)*(c41 m) -- |
      - (c11 m)*(c23 m)*(c32 m)*(c44 m) -- |
      - (c11 m)*(c24 m)*(c33 m)*(c42 m) -- |
      - (c11 m)*(c22 m)*(c34 m)*(c43 m) -- /
      - (c12 m)*(c23 m)*(c34 m)*(c41 m) -- \
      - (c12 m)*(c24 m)*(c31 m)*(c43 m) -- | (4)
      - (c13 m)*(c24 m)*(c32 m)*(c41 m) -- |
      - (c13 m)*(c21 m)*(c34 m)*(c42 m) -- |
      - (c14 m)*(c23 m)*(c31 m)*(c42 m) -- |
      - (c14 m)*(c21 m)*(c32 m)*(c43 m) -- /

  invert m = do
    let d = det m
    if d == 0
      then Nothing
      else undefined
