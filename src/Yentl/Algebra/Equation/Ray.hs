{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yentl.Algebra.Equation.Ray (
  RayEquation(), reVertex,
  RayType(..), rayType,

  -- constructors
  reVertexPoint
) where


import Yentl.Algebra.Number.Ord
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.System2
import Yentl.Algebra.Equation.LinearForm
import Yentl.Algebra.Equation.Segment


{---------------}
{- RayEquation -}
{---------------}

data RayEquation a = RE
  { reOx :: a
  , reOy :: a
  , reAx :: a
  , reAy :: a
  }


reVertex :: RayEquation a -> (a,a)
reVertex eq = (reOx eq, reOy eq)


reToward :: RayEquation a -> (a,a)
reToward eq = (reAx eq, reAy eq)


data RayType a
  = TrivialRay (a,a)
  | OrdinaryRay (LinearForm a)


rayType :: (Eq a, Num a) => RayEquation a -> RayType a
rayType eq =
  let
    o = reVertex eq
    a = reToward eq
  in
    if o == a
      then TrivialRay o
      else case lfPointPoint o a of
        Nothing -> error "rayType"
        Just ell -> OrdinaryRay ell



{-------------}
{- Instances -}
{-------------}

{- IsSolutionOf -}

instance (Ord a, Num a) => EquationIn2Var a (RayEquation a) where
  isSolutionOf2 (x,y) ray =
    let (ox,oy) = reVertex ray in
    let (ax,ay) = reToward ray in
    and
      [ (oy - ay)*x + (ax - ox)*y + ox*ay - ax*oy == 0
      , onSameSideOf1 (x,ax) ox
      , onSameSideOf1 (y,ay) oy
      ]



{----------------}
{- Constructors -}
{----------------}

reVertexPoint :: (a,a) -> (a,a) -> RayEquation a
reVertexPoint (ox,oy) (ax,ay) = RE
  { reOx = ox
  , reOy = oy
  , reAx = ax
  , reAy = ay
  }



{-----------}
{- Systems -}
{-----------}

{- Point and Ray -}

instance (Ord a, Num a)
  => Solve2var1sol a ((a,a), RayEquation a) where
  solve2var1sol (p, ray) = if isSolutionOf2 p ray
    then Just p
    else Nothing


instance (Ord a, Num a)
  => Solve2var1sol a (RayEquation a, (a,a)) where
  solve2var1sol (ray, p) = solve2var1sol (p, ray)



{- Ray and Linear Form -}

instance (Ord a, Num a, Fractional a)
  => Solve2var1sol a (RayEquation a, LinearForm a) where
  solve2var1sol (ray, ell) = case rayType ray of
    TrivialRay p -> solve2var1sol (p, ell)

    OrdinaryRay ell2 -> do
      w <- solve2var1sol (ell, ell2)
      solve2var1sol (w, ray)


instance (Ord a, Num a, Fractional a)
  => Solve2var1sol a (LinearForm a, RayEquation a) where
  solve2var1sol (ell, ray) = solve2var1sol (ray, ell)



{- Ray and Segment -}

instance (Ord a, Num a, Fractional a)
  => Solve2var1sol a (RayEquation a, SegmentEquation a) where
  solve2var1sol (ray, seg) = case rayType ray of
    TrivialRay p -> solve2var1sol (p, seg)

    OrdinaryRay ell -> do
      w <- solve2var1sol (ell, seg)
      solve2var1sol (w, ray)


instance (Ord a, Num a, Fractional a)
  => Solve2var1sol a (SegmentEquation a, RayEquation a) where
  solve2var1sol (seg, ray) = solve2var1sol (ray, seg)


{- Two Rays -}

instance (Ord a, Num a, Fractional a)
  => Solve2var1sol a (RayEquation a, RayEquation a) where
  solve2var1sol (ray1, ray2) = case rayType ray1 of
    TrivialRay p -> solve2var1sol (p, ray2)

    OrdinaryRay ell -> do
      w <- solve2var1sol (ell, ray2)
      solve2var1sol (w, ray1)
