{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yentl.Algebra.Equation.Segment (
  SegmentEquation(), seCoords,
  SegmentType(..), segmentType,

  -- constructors
  seEndpoints
) where


import Yentl.Algebra.Classes
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.System2
import Yentl.Algebra.Equation.LinearForm


{--------------------}
{- Segment Equation -}
{--------------------}

data SegmentEquation a = SE
  { ptAx :: a
  , ptAy :: a
  , ptBx :: a
  , ptBy :: a
  } deriving Eq


seCoords :: SegmentEquation a -> ((a,a),(a,a))
seCoords eq = ((ptAx eq, ptAy eq),(ptBx eq, ptBy eq))


data SegmentType a
  = TrivialSegment  (a,a)          -- endpoints are equal
  | OrdinarySegment (LinearForm a) -- endpoints are not equal


segmentType :: (Eq a, Num a) => SegmentEquation a -> SegmentType a
segmentType eq =
  let (p,q) = seCoords eq in
  if p == q
    then TrivialSegment p
    else case lfPointPoint p q of
      Nothing  -> error "segmentType"
      Just ell -> OrdinarySegment ell



{-------------}
{- Instances -}
{-------------}

{- Show -}

instance (Show a) => Show (SegmentEquation a) where
  show eq =
    let (a,b) = seCoords eq in
    concat ["seg(" ++ show a ++ "," ++ show b ++ ")"]



{- IsSolutionOf -}

instance (Ord a, Num a) => EquationIn2Var a (SegmentEquation a) where
  isSolutionOf2 (x,y) seg =
    let ((ax,ay),(bx,by)) = seCoords seg in
    and
      [ (ay - by)*x + (bx - ax)*y + ax*by - bx*ay == 0
      , (ax <= x && x <= bx) || (bx <= x && x <= ax)
      , (ay <= y && y <= by) || (by <= y && y <= ay)
      ]



{----------------}
{- Constructors -}
{----------------}

-- given two endpoints
seEndpoints :: (a,a) -> (a,a) -> SegmentEquation a
seEndpoints (ax,ay) (bx,by) = SE
  { ptAx = ax
  , ptAy = ay
  , ptBx = bx
  , ptBy = by
  }



{-----------}
{- Systems -}
{-----------}

{- Point and Segment -}

instance (Ord a, Num a)
  => Solve2var1sol a ((a,a), SegmentEquation a) where
  solve2var1sol (p, seg) = if isSolutionOf2 p seg
    then Just p
    else Nothing


instance (Ord a, Num a)
  => Solve2var1sol a (SegmentEquation a, (a,a)) where
  solve2var1sol (seg, p) = solve2var1sol (p, seg)



{- Segment and Linear Form -}

instance (Ord a, Num a, Fractional a)
  => Solve2var1sol a (SegmentEquation a, LinearForm a) where
  solve2var1sol (seg, ell) = case segmentType seg of
    TrivialSegment p -> solve2var1sol (p, ell)

    OrdinarySegment ell2 -> do
      w <- solve2var1sol (ell, ell2)
      solve2var1sol (w, seg)


instance (Ord a, Num a, Fractional a)
  => Solve2var1sol a (LinearForm a, SegmentEquation a) where
  solve2var1sol (ell, seg) = solve2var1sol (seg, ell)



{- Two Segments -}

instance (Ord a, Num a, Fractional a)
  => Solve2var1sol a (SegmentEquation a, SegmentEquation a) where
  solve2var1sol (seg1, seg2) = case segmentType seg1 of
    TrivialSegment p -> solve2var1sol (p, seg2)

    OrdinarySegment ell -> do
      w <- solve2var1sol (ell, seg2)
      solve2var1sol (w, seg2)
