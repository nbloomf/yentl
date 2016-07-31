{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yentl.Algebra.Equation.MinorArc (
  MinorArcEquation(), maCenter, maCoords,
  MinorArcType(..), minorArcType, maMidpoint,

  -- constructors
  maCenterEndpoints
) where


import Yentl.Algebra.Classes
import Yentl.Algebra.Data.Mat2
import Yentl.Algebra.Data.Mat3
import Yentl.Algebra.Data.AtMost
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.System2
import Yentl.Algebra.Equation.LinearForm
import Yentl.Algebra.Equation.Segment
import Yentl.Algebra.Equation.Ray
import Yentl.Algebra.Equation.Circle


{--------------------}
{- MinorArcEquation -}
{--------------------}

data MinorArcEquation a = MA
  { maOx :: a, maOy :: a
  , maAx :: a, maAy :: a
  , maBx :: a, maBy :: a
  }


maCenter :: MinorArcEquation a -> (a,a)
maCenter eq = (maOx eq, maOy eq)


maCoords :: MinorArcEquation a -> ((a,a),(a,a))
maCoords eq = ((maAx eq, maAy eq), (maBx eq, maBy eq))


data MinorArcType a
  = TrivialMinorArc  (a,a)              -- endpoints are equal
  | OrdinaryMinorArc (CircleEquation a) -- endpoints are not equal

minorArcType :: (Ord a, Num a, SquareRoot a)
  => MinorArcEquation a -> MinorArcType a
minorArcType eq =
  let
    o = maCenter eq
    (a,b) = maCoords eq
  in
    if a == b || a == o
      then TrivialMinorArc a
      else OrdinaryMinorArc $ ceCenterPoint o a



{-------------}
{- Instances -}
{-------------}

{- Show -}

instance (Show a) => Show (MinorArcEquation a) where
  show eq =
    let o = maCenter eq in
    let (a,b) = maCoords eq in
    concat ["minorarc(" ++ show a ++ "," ++ show o ++ "," ++ show b ++ ")"]



{- IsSolutionOf -}

instance (Ord a, Num a, SquareRoot a) => EquationIn2Var a (MinorArcEquation a) where
  isSolutionOf2 p arc = case minorArcType arc of
    TrivialMinorArc q -> p == q

    OrdinaryMinorArc circ ->
      let (a,b) = maCoords arc in
      let o = maCenter arc in
      and
        [ isSolutionOf2 p circ
        , (orientation a b o) /= (orientation a b p)
        ]



{----------------}
{- Constructors -}
{----------------}

maCenterEndpoints :: (Eq a, Num a, Fractional a)
  => (a,a) -> ((a,a),(a,a)) -> Maybe (MinorArcEquation a)
maCenterEndpoints o@(ox,oy) (a@(ax,ay),b@(bx,by)) = do
  if a == o || b == o
    then Nothing
    else if a == b
      then Just $ MA
        { maOx = ox, maOy = oy
        , maAx = ax, maAy = ay
        , maBx = ax, maBy = ay
        }
      else do
        let da = (ax - ox)^2 + (ay - oy)^2
        let db = (bx - ox)^2 + (by - oy)^2
        if da /= db
          then Nothing
          else if o == midpoint a b
            then Nothing
            else Just $ MA
              { maOx = ox, maOy = oy
              , maAx = ax, maAy = ay
              , maBx = bx, maBy = by
              }


maMidpoint :: (Ord a, Num a, Fractional a, SquareRoot a)
  => MinorArcEquation a -> (a,a)
maMidpoint eq = case minorArcType eq of
  TrivialMinorArc p -> p

  OrdinaryMinorArc _ ->
    let
      (a,b) = maCoords eq
      Just ell = lfPointFoot a (midpoint a b)
      Only1of2 m = solve2var2sol (ell, eq)
    in m


{-----------}
{- Systems -}
{-----------}



{- Point and Minor Arc -}

instance (Ord a, Num a, SquareRoot a)
  => Solve2var1sol a ((a,a), MinorArcEquation a) where
  solve2var1sol (p, arc) = if isSolutionOf2 p arc
    then Just p
    else Nothing

instance (Ord a, Num a, SquareRoot a)
  => Solve2var1sol a (MinorArcEquation a, (a,a)) where
  solve2var1sol (arc, p) = solve2var1sol (p, arc)



{- Minor Arc and Linear Form -}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (MinorArcEquation a, LinearForm a) where
  solve2var2sol (arc, ell) = case minorArcType arc of
    TrivialMinorArc p -> if isSolutionOf2 p ell
      then Only1of2 p
      else Only0of2

    OrdinaryMinorArc circ ->
      let ws = solve2var2sol (ell, circ) in
      filterAtMost (`isSolutionOf2` arc) ws


instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (LinearForm a, MinorArcEquation a) where
  solve2var2sol (ell, arc) = solve2var2sol (arc, ell)



{- Minor Arc and Segment -}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (MinorArcEquation a, SegmentEquation a) where
  solve2var2sol (arc, seg) = case segmentType seg of
    TrivialSegment p -> if isSolutionOf2 p arc
      then Only1of2 p
      else Only0of2

    OrdinarySegment ell ->
      let ws = solve2var2sol (ell, arc) in
      filterAtMost (`isSolutionOf2` seg) ws


instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (SegmentEquation a, MinorArcEquation a) where
  solve2var2sol (seg, arc) = solve2var2sol (arc, seg)



{- Minor Arc and Ray -}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (MinorArcEquation a, RayEquation a) where
  solve2var2sol (arc, ray) = case rayType ray of
    TrivialRay p -> if isSolutionOf2 p arc
      then Only1of2 p
      else Only0of2

    OrdinaryRay ell ->
      let ws = solve2var2sol (ell, arc) in
      filterAtMost (`isSolutionOf2` ray) ws


instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (RayEquation a, MinorArcEquation a) where
  solve2var2sol (ray, arc) = solve2var2sol (arc, ray)



{- Minor Arc and Circle -}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (MinorArcEquation a, CircleEquation a) where
  solve2var2sol (arc, circ) = case minorArcType arc of
    TrivialMinorArc p -> if isSolutionOf2 p circ
      then Only1of2 p
      else Only0of2

    OrdinaryMinorArc circ2 ->
      let ws = solve2var2sol (circ, circ2) in
      filterAtMost (`isSolutionOf2` arc) ws


instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (CircleEquation a, MinorArcEquation a) where
  solve2var2sol (circ, arc) = solve2var2sol (arc, circ)



{- Two Minor Arcs -}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve2var2sol a (MinorArcEquation a, MinorArcEquation a) where
  solve2var2sol (arc1, arc2) = case minorArcType arc1 of
    TrivialMinorArc p -> if isSolutionOf2 p arc2
      then Only1of2 p
      else Only0of2

    OrdinaryMinorArc circ ->
      let ws = solve2var2sol (circ, arc2) in
      filterAtMost (`isSolutionOf2` arc1) ws



