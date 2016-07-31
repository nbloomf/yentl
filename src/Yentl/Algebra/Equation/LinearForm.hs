{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yentl.Algebra.Equation.LinearForm (
  LinearForm(), lfCoefs, lfTwoSolutionsOf,
  xIntercept, solveForX, solveForY, lfSlope,

  -- constructors
  lfStandardForm, lfPointPoint, lfPointSlope,
  lfPointFoot, lfPerpBisector, lfPointPerp,

  -- predicates
  lfIsVertical, lfIsHorizontal,
  lfParallel, lfPerpendicular
) where

import Yentl.Algebra.Classes
import Yentl.Algebra.Number.Slope
import Yentl.Algebra.Data.Mat2
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.System2



{--------------}
{- LinearForm -}
{--------------}

-- ax + by + c = 0
data LinearForm a = LinearForm
  { coefA :: a
  , coefB :: a
  , coefC :: a
  }


lfSlope :: (Eq a, Num a) => LinearForm a -> Slope a
lfSlope ell =
  let Just m = slope (coefB ell) (-(coefA ell)) in m


-- ax + by + c = 0  ==>  (a,b,c)
lfCoefs :: LinearForm a -> (a,a,a)
lfCoefs x = (coefA x, coefB x, coefC x)


solveForX :: (Eq a, Num a, Fractional a) => LinearForm a -> a -> Maybe a
solveForX ell y =
  let (a,b,c) = lfCoefs ell in
  if a == 0
    then Nothing
    else Just $ (-b*y - c) / a

xIntercept :: (Eq a, Num a, Fractional a)
  => LinearForm a -> Maybe a
xIntercept ell = solveForX ell 0

solveForY :: (Eq a, Num a, Fractional a) => LinearForm a -> a -> Maybe a
solveForY ell x =
  let (a,b,c) = lfCoefs ell in
  if b == 0
    then Nothing
    else Just $ (-a*x - c) / b


lfTwoSolutionsOf :: (Eq a, Num a, Fractional a)
  => LinearForm a -> ((a,a),(a,a))
lfTwoSolutionsOf ell = if lfIsVertical ell
  then
    let Just c = solveForX ell 0 in
    ((c,0),(c,1))
  else
    let Just a = solveForY ell 0 in
    let Just b = solveForY ell 1 in
    ((0,a),(1,b))



{-------------}
{- Instances -}
{-------------}

{- Eq -}

-- equivalent lines are constant multiples of each other
instance (Eq a, Num a) => Eq (LinearForm a) where
  l1 == l2 = and
    [ (coefA l1)*(coefB l2) == (coefB l1)*(coefA l2)
    , (coefB l1)*(coefC l2) == (coefC l1)*(coefB l2)
    , (coefC l1)*(coefA l2) == (coefA l1)*(coefC l2)
    ]



{- Show -}

instance (Show a) => Show (LinearForm a) where
  show f = concat
    [ show (coefA f), "x + "
    , show (coefB f), "y + "
    , show (coefC f), " = 0"
    ]



{- EquationIn2Var -}

instance (Eq a, Num a) => EquationIn2Var a (LinearForm a) where
  isSolutionOf2 (x,y) ell =
    let (a,b,c) = lfCoefs ell in
    x*a + y*b + c == 0



{----------------}
{- Constructors -}
{----------------}

-- given coefficients
lfStandardForm :: (Eq a, Num a)
  => a -> a -> a -> Maybe (LinearForm a)
lfStandardForm a b c = if (a,b) == (0,0)
  then Nothing
  else Just $ LinearForm
    { coefA = a
    , coefB = b
    , coefC = c
    }


-- given two points
lfPointPoint :: (Eq a, Num a)
  => (a,a) -> (a,a) -> Maybe (LinearForm a)
lfPointPoint (x1,y1) (x2,y2) = do
  let a = y1 - y2
  let b = x2 - x1
  let c = x1*y2 - x2*y1
  lfStandardForm a b c


-- given a point and the slope 
lfPointSlope :: (Eq a, Num a)
  => (a,a) -> Slope a -> Maybe (LinearForm a)
lfPointSlope (x,y) s = do
  let a = deltaY s
  let b = negate (deltaX s)
  let c = (deltaX s)*y - (deltaY s)*x
  lfStandardForm a b c


-- given a point and the foot of that point
lfPointFoot :: (Eq a, Num a, Fractional a)
  => (a,a) -> (a,a) -> Maybe (LinearForm a)
lfPointFoot (x1,y1) (x2,y2) = do
  m <- slope (x2 - x1) (y2 - y1)
  lfPointSlope (x2,y2) (perp m)


-- given two points which it perp bisects
lfPerpBisector :: (Eq a, Num a, Fractional a)
  => (a,a) -> (a,a) -> Maybe (LinearForm a)
lfPerpBisector (x1,y1) (x2,y2) = do
  let f = ((x1 + x2)/2, (y1 + y2)/2)
  lfPointFoot (x1,y1) f


-- given a point and a line perpendicular
lfPointPerp :: (Eq a, Num a, Fractional a)
  => (a,a) -> LinearForm a -> Maybe (LinearForm a)
lfPointPerp p ell = do
  let m = perp $ lfSlope ell
  lfPointSlope p m



{--------------}
{- Predicates -}
{--------------}

lfIsVertical :: (Eq a, Num a)
  => LinearForm a -> Bool
lfIsVertical ell = let (_,b,_) = lfCoefs ell in b == 0


lfIsHorizontal :: (Eq a, Num a)
  => LinearForm a -> Bool
lfIsHorizontal ell = let (a,_,_) = lfCoefs ell in a == 0


lfParallel :: (Eq a, Num a, Fractional a)
  => LinearForm a -> LinearForm a -> Bool
lfParallel ell1 ell2 =
  let (a1,b1,_) = lfCoefs ell1 in
  let (a2,b2,_) = lfCoefs ell2 in
  a1*b2 - a2*b1 == 0


lfPerpendicular :: (Eq a, Num a, Fractional a)
  => LinearForm a -> LinearForm a -> Bool
lfPerpendicular ell1 ell2 =
  let (a1,b1,_) = lfCoefs ell1 in
  let (a2,b2,_) = lfCoefs ell2 in
  a1*a2 + b1*b2 == 0



{-----------}
{- Systems -}
{-----------}

{- Point and Linear Form -}

instance (Eq a, Num a)
  => Solve2var1sol a ((a,a), LinearForm a) where
  solve2var1sol (p, ell) = if isSolutionOf2 p ell
    then Just p
    else Nothing


instance (Eq a, Num a)
  => Solve2var1sol a (LinearForm a, (a,a)) where
  solve2var1sol (ell, p) = solve2var1sol (p, ell)



{- Two Linear Forms -}

instance (Eq a, Num a, Fractional a)
  => Solve2var1sol a (LinearForm a, LinearForm a) where
  solve2var1sol (ell1, ell2) = do
    let (a1,b1,c1) = lfCoefs ell1
    let (a2,b2,c2) = lfCoefs ell2
    let v = (-c1, -c2)
    case invert $ fromRows2 ((a1,b1),(a2,b2)) of
      Nothing -> Nothing
      Just m  -> Just $ m %. v
