module Yentl.Algebra.Equation.Optimize (
  projectPtLF, projectPtCE, tangentPtCE
) where

import Yentl.Algebra.Classes
import Yentl.Algebra.Data.AtMost
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.LinearForm
import Yentl.Algebra.Equation.Circle
import Yentl.Algebra.Equation.System2
import Yentl.Algebra.Equation.System

-- find the foot of a point on a line
projectPtLF :: (Eq a, Num a, Fractional a)
  => (a,a) -> LinearForm a -> (a,a)
projectPtLF p ell =
  if isSolutionOf2 p ell
    then p
    else
      let
        Just ell' = lfPointPerp p ell
        Just q = solve2var1sol (ell, ell')
      in q


-- find point on circle which minimizes distance to given point
projectPtCE :: (Ord a, Num a, Fractional a, SquareRoot a)
  => (a,a) -> CircleEquation a -> Maybe (a,a)
projectPtCE x circ = do
  let (h,k,r) = ceCoefs circ
  let o = (h,k)
  if r==0 || x == o
    then Nothing
    else do
      let Just t = root2 ((o -. x)`dot`(o -. x))
      Just $ interpolate (r/t) o x


tangentPtCE :: (Ord a, Num a, Fractional a, SquareRoot a)
  => (a,a) -> CircleEquation a -> AtMost2 (a,a)
tangentPtCE (ax,ay) circ1 =
  let
    (ox,oy,_) = ceCoefs circ1
    (mx,my) = ((ax+ox)/2, (ay+oy)/2)
    circ2 = ceCenterPoint (mx,my) (ax,ay)
  in solve2var2sol (circ1, circ2)
