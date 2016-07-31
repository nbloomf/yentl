{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Yentl.Algebra.Equation.System (
) where

import Data.Maybe (catMaybes)
import Data.List (nub)

import Yentl.Algebra.Classes
import Yentl.Algebra.Number.Sign
import Yentl.Algebra.Data.Mat2
import Yentl.Algebra.Data.AtMost
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.System2
import Yentl.Algebra.Equation.LinearForm
import Yentl.Algebra.Equation.Segment
import Yentl.Algebra.Equation.Box
import Yentl.Algebra.Equation.Ray
import Yentl.Algebra.Equation.Quadratic
import Yentl.Algebra.Equation.Circle
import Yentl.Algebra.Equation.MinorArc



{-------}
{- Box -}
{-------}

{- Point and Box -}

instance (Ord a, Num a)
  => Solve2var1sol a ((a,a), Box a) where
  solve2var1sol (p, box) = if isSolutionOf2 p box
    then Just p
    else Nothing


instance (Ord a, Num a)
  => Solve2var1sol a (Box a, (a,a)) where
  solve2var1sol (box, p) = solve2var1sol (p, box)


{- Box and Linear Form -}

instance (Ord a, Num a, Fractional a)
  => Solve2var2sol a (Box a, LinearForm a) where
  solve2var2sol (box, ell) =
    let
      (s1,s2,s3,s4) = boxSides box
      a1 = solve2var1sol (ell,s1)
      a2 = solve2var1sol (ell,s2)
      a3 = solve2var1sol (ell,s3)
      a4 = solve2var1sol (ell,s4)
      Just t = fromList $ nub $ catMaybes [a1,a2,a3,a4]
    in t


instance (Ord a, Num a, Fractional a)
  => Solve2var2sol a (LinearForm a, Box a) where
  solve2var2sol (ell, box) = solve2var2sol (box, ell)


{- Box and Segment -}

instance (Ord a, Num a, Fractional a)
  => Solve2var2sol a (Box a, SegmentEquation a) where
  solve2var2sol (box, seg) = case segmentType seg of
    TrivialSegment p -> case solve2var1sol (p, box) of
      Nothing -> Only0of2
      Just w  -> Only1of2 w

    OrdinarySegment ell -> do
      let ws = solve2var2sol (ell, box)
      filterAtMost (`isSolutionOf2` seg) ws


instance (Ord a, Num a, Fractional a)
  => Solve2var2sol a (SegmentEquation a, Box a) where
  solve2var2sol (seg, box) = solve2var2sol (box, seg)


{- Box and Ray -}

instance (Ord a, Num a, Fractional a)
  => Solve2var2sol a (RayEquation a, Box a) where
  solve2var2sol (ray, box) = case rayType ray of
    TrivialRay p -> case solve2var1sol (p, box) of
      Nothing -> Only0of2
      Just w  -> Only1of2 w

    OrdinaryRay ell -> do
      let ws = solve2var2sol (ell, box)
      filterAtMost (`isSolutionOf2` ray) ws


instance (Ord a, Num a, Fractional a)
  => Solve2var2sol a (Box a, RayEquation a) where
  solve2var2sol (box, ray) = solve2var2sol (ray, box)


{- Box and Circle -}


{- Box and Minor Arc -}

