{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Yentl.Algebra.Equation.Curve (
  Curve, along
) where


import Yentl.Algebra.Data.AtMost
import Yentl.Algebra.Classes
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.LinearForm
import Yentl.Algebra.Equation.Segment
import Yentl.Algebra.Equation.MinorArc
import Yentl.Algebra.Equation.System
import Yentl.Algebra.Equation.System2
import Yentl.Algebra.Equation.Optimize


{- Curve -}

class Curve a t | t -> a where
  along :: (Ord a, Num a, Fractional a) => a -> t -> (a,a)

  alongBy :: (Ord a, Num a) => a -> (a,a) -> (a,a) -> t -> Maybe (a,a)



{- Linear Form -}

instance (Eq a, Num a) => Curve a (LinearForm a) where
  along t eq =
    let (p,q) = lfTwoSolutionsOf eq in
    interpolate t p q

  alongBy t p q eq =
    if p == q
      then Nothing
      else if not (isSolutionOf2 p eq && isSolutionOf2 q eq)
        then Nothing
        else Just $ interpolate t p q



{- Segment -}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Curve a (SegmentEquation a) where
  along t eq =
    let
      w = if t == 0
        then 1/2
        else
          let Just r = root2 $ t^2 + 4 in
          (t - 2 + r) / (2*t)

      (a,b) = seCoords eq
    in a +. (w @. (b -. a))

  alongBy t p q eq =
    if p == q
      then Nothing
      else if not (isSolutionOf2 p eq && isSolutionOf2 q eq)
        then Nothing
        else do
          let (a,b) = seCoords eq
          if p == a || p == b || q == a || q == b
            then Nothing
            else do
              let
                (u,v) = if isSolutionOf2 p (seEndpoints a q)
                          then (a,b)
                          else (b,a)

                w = if t == 0
                      then 1/2
                      else
                        let Just r = root2 $ t^2 + 4 in
                        (t - 2 + r) / (2*t)

                alpha = 1/2
                beta = let Just z = root2 5 in (-1 + z)/2

              if t < 0
                then Just $ u +. ((w/alpha) @. (p -. u))
                else if t < 1
                  then Just $ p +. (((w - alpha)/(beta - alpha)) @. (q -. p))
                  else Just $ q +. (((w - beta)/(1 - beta)) @. (v -. q))


{- MinorArc -}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Curve a (MinorArcEquation a) where
  along t eq =
    let
      (a,b) = maCoords eq
      o = maCenter eq
      seg = seEndpoints a b
      c = along t seg
      Just ell = lfPointPoint o c
      Only1of2 p = solve2var2sol (eq, ell)
    in p

  alongBy t p q eq =
    if p == q
      then Nothing
      else if not (isSolutionOf2 p eq && isSolutionOf2 q eq)
        then Nothing
        else case minorArcType eq of
          TrivialMinorArc _ -> Nothing
          OrdinaryMinorArc circ -> do
            let (a,b) = maCoords eq
            let o = maCenter eq
            let m = midpoint a b
            let Just ell = lfPointPoint o m
            let Only1of2 n = solve2var2sol (ell, eq)
            let Just ell2 = lfPointFoot o n
            let a' = projectPtLF a ell2
            let b' = projectPtLF b ell2
            let seg = seEndpoints a' b'
            let p' = projectPtLF p ell2
            let q' = projectPtLF q ell2
            x <- alongBy t p' q' seg
            let Just ell3 = lfPointPerp x ell2
            let Only1of2 y = solve2var2sol (ell3, eq)
            Just y
