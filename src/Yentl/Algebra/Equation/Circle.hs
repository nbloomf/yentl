{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Yentl.Algebra.Equation.Circle (
  CircleEquation(),
  ceCoefs,
  ceCenterOf,
  ceRadiusOf,

  -- constructors
  ceCenterRadius,
  ceCenterPoint,
  ceDiameter,
  cePointPointPoint,

  -- misc
  ceInvertPoint
) where

import Yentl.Algebra.Classes
import Yentl.Algebra.Data.Mat2
import Yentl.Algebra.Data.AtMost
import Yentl.Algebra.Number.Sign
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.System2
import Yentl.Algebra.Equation.Quadratic
import Yentl.Algebra.Equation.LinearForm
import Yentl.Algebra.Equation.Segment
import Yentl.Algebra.Equation.Ray

{------------------}
{- CircleEquation -}
{------------------}

-- (x-h)^2 + (y-k)^2 = r^2
data CircleEquation a = CE
  { hCoef :: a
  , kCoef :: a
  , rCoef :: a
  } deriving Show

ceCoefs :: CircleEquation a -> (a,a,a)
ceCoefs x = (hCoef x, kCoef x, rCoef x)

ceCenterOf :: (Num a) => CircleEquation a -> (a,a)
ceCenterOf eq = (hCoef eq, kCoef eq)

ceRadiusOf :: (Num a) => CircleEquation a -> a
ceRadiusOf eq = abs $ rCoef eq





{-------------}
{- Instances -}
{-------------}

instance (Eq a, Num a) => Eq (CircleEquation a) where
  eq1 == eq2 =
    let (h1,k1,r1) = ceCoefs eq1 in
    let (h2,k2,r2) = ceCoefs eq2 in
    (h1 == h2) && (k1 == k2) && ((abs r1) == (abs r2))

instance (Eq a, Num a) => EquationIn2Var a (CircleEquation a) where
  isSolutionOf2 (x,y) eq = 
    let (h,k,r) = ceCoefs eq in
    (x - h)^2 + (y - k)^2 == r^2



{----------------}
{- Constructors -}
{----------------}

-- center point and radius
ceCenterRadius :: (Ord a, Num a)
  => (a,a) -> a -> CircleEquation a
ceCenterRadius (h,k) r =
  CE { hCoef = h, kCoef = k, rCoef = abs r }


-- center point and point on circle
ceCenterPoint :: (Ord a, Num a, SquareRoot a)
  => (a,a) -> (a,a) -> CircleEquation a
ceCenterPoint (h,k) (u,v) =
  let Just r = root2 $ (u-h)^2 + (v-k)^2 in
  CE { hCoef = h, kCoef = k, rCoef = r }


-- endpoints of a diameter
ceDiameter :: (Ord a, Num a, Fractional a, SquareRoot a)
  => (a,a) -> (a,a) -> CircleEquation a
ceDiameter a b = ceCenterPoint (midpoint a b) a


-- three points on circle
cePointPointPoint :: (Ord a, Num a, Fractional a, SquareRoot a)
  => (a,a) -> (a,a) -> (a,a) -> Maybe (CircleEquation a)
cePointPointPoint a b c = do
  ell1 <- lfPerpBisector a b
  ell2 <- lfPerpBisector b c
  o <- solve2var1sol (ell1, ell2)
  Just $ ceCenterPoint o a



{- Misc -}

-- inverse of a point wrt circle
ceInvertPoint :: (Ord a, Num a, Fractional a, SquareRoot a)
  => CircleEquation a -> (a,a) -> Maybe (a,a)
ceInvertPoint circ (x,y) = do
  let (h,k) = ceCenterOf circ
  if (x,y) == (h,k)
    then Nothing
    else do
      let r = ceRadiusOf circ
      if r == 0
        then Nothing
        else do
          let a = r^2 / ((x-h)^2 + (y-k)^2)
          Just (a*(x-h)+h, a*(y-k)+k)


{-----------}
{- Systems -}
{-----------}


{- Point and Circle -}

instance (Eq a, Num a)
  => Solve2var1sol a ((a,a), CircleEquation a) where
  solve2var1sol (p, circ) = if isSolutionOf2 p circ
    then Just p
    else Nothing

instance (Eq a, Num a)
  => Solve2var1sol a (CircleEquation a, (a,a)) where
  solve2var1sol (circ, p) = solve2var1sol (p, circ)


{- Circle and Linear Form -}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (LinearForm a, CircleEquation a) where
  solve2var2sol (ell, circ) =
    let
      (a,b,c) = lfCoefs ell
      (h,k,r) = ceCoefs circ
    in
      if a == 0
        then do
          let yOf x = (-c - a*x)/b
          let disc = r^2 - ((c/b) + k)^2
          case signOf disc of
            Negative -> Only0of2
            Zero -> Only1of2 (h, yOf h)
            Positive -> do
              case root2 disc of
                Nothing -> error "solveLFCE (1)"
                Just t -> do
                  let x1 = h + t
                  let x2 = h - t
                  Only2of2 (x1, yOf x1) (x2, yOf x2)
        else do
          let xOf y = (-c - b*y)/a
          let alpha = (b/a)^2 + 1
          let beta = 2 * ((b/a)*((c/a)+h) - k)
          let gamma = ((c/a)+h)^2 + k^2 - r^2
          case qeFromCoefs alpha beta gamma of
            Nothing -> error "solveLFCE (2)"
            Just eq ->
              case solve1var2sol eq of
                Only0of2       -> Only0of2
                Only1of2 y     -> Only1of2 (xOf y, y)
                Only2of2 y1 y2 -> Only2of2 (xOf y1, y1) (xOf y2, y2)


instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (CircleEquation a, LinearForm a) where
  solve2var2sol (circ, ell) = solve2var2sol (ell, circ)



{- Circle and Segment -}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (SegmentEquation a, CircleEquation a) where
  solve2var2sol (seg, circ) = case segmentType seg of
    TrivialSegment p -> injectAtMost $ solve2var1sol (p, circ)

    OrdinarySegment ell ->
      let ws = solve2var2sol (ell, circ) in
      filterAtMost (`isSolutionOf2` seg) ws


instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (CircleEquation a, SegmentEquation a) where
  solve2var2sol (circ, seg) = solve2var2sol (seg, circ)



{- Circle and Ray -}


{- Two Circles -}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (CircleEquation a, CircleEquation a) where
  solve2var2sol (circ1, circ2) =
    let
      (h1,k1,r1) = ceCoefs circ1
      (h2,k2,r2) = ceCoefs circ2
      a = 2*(h2-h1)
      b = 2*(k2-k1)
      c = h1^2+k1^2+r2^2-h2^2-k2^2-r1^2
    in
      case lfStandardForm a b c of
        Nothing  -> Only0of2
        Just ell -> solve2var2sol (ell, circ2)
