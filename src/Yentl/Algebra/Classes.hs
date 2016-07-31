{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Yentl.Algebra.Classes (
  RealField,

  SquareRoot, root2,

  Module, zero, (+.), (@.), minus,
  modSum, (-.), interpolate, midpoint,

  Vector, dot, dotSq, distSq, norm, distance,

  SquareMatrix, (*.), one, det, invert,

  MatMult, (%.),

  IsOn, isOn,

  IsInside, isInside
) where


class (Ord a, Num a, Fractional a) => RealField a


{--------------}
{- SquareRoot -}
{--------------}

class SquareRoot a where
  root2 :: a -> Maybe a



{----------}
{- Module -}
{----------}

class Module r t | t -> r where
  zero  :: t
  (+.)  :: t -> t -> t -- plus
  (@.)  :: r -> t -> t -- scale
  minus :: t -> t

instance (Num a) => Module a (a,a) where
  zero = (0,0)
  (x1,y1) +. (x2,y2) = (x1 + x2, y1 + y2)
  a @. (x,y) = (a*x, a*y)
  minus (x,y) = (-x, -y)

-- sigma
modSum :: (Module r t) => [t] -> t
modSum = foldr (+.) zero

-- subtraction
(-.) :: (Module r t) => t -> t -> t
a -. b = a +. (minus b)

interpolate :: (Module r t, Num r) => r -> t -> t -> t
interpolate t x y =  x +. (t @. (y -. x))

midpoint :: (Module r t, Num r, Fractional r) => t -> t -> t
midpoint = interpolate (1/2)



{----------}
{- Vector -}
{----------}

class Vector r t | t -> r where
  dot :: t -> t -> r

instance (Num a) => Vector a (a,a) where
  dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

-- dot square
dotSq :: (Vector r t) => t -> r
dotSq x = dot x x

-- square of distance
distSq :: (Module r t, Vector r t) => t -> t -> r
distSq x y = dotSq (y -. x)


norm :: (Vector r t, SquareRoot r) => t -> r
norm x = case root2 $ dotSq x of
  Nothing -> error "norm"
  Just y  -> y

normalize :: (Eq t, Module r t, Vector r t, SquareRoot r, Fractional r) => t -> t
normalize x = if x == zero
  then zero
  else (recip $ norm x) @. x

-- metric induced by dot
distance :: (Module r t, Vector r t, SquareRoot r) => t -> t -> r
distance x y = norm (y -. x)




{----------------}
{- SquareMatrix -}
{----------------}

class SquareMatrix r t | t -> r where
  (*.)   :: t -> t -> t
  one    :: t
  det    :: t -> r
  invert :: t -> Maybe t



{-----------}
{- MatMult -}
{-----------}

class MatMult a b where
  (%.) :: a -> b -> b



{--------}
{- IsOn -}
{--------}

class IsOn a t u | t -> a, u -> a where
  isOn :: t -> u -> Bool

class IsInside a t u | t -> a, u -> a where
  isInside :: t -> u -> Bool
