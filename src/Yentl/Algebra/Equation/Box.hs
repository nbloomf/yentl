{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yentl.Algebra.Equation.Box (
  Box(), lowerLeft, upperRight, makeBox, boundingBox, boxCorners, boxSides, isInsideBox
) where

import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.Segment

data Box a = Box
  { xmin :: a
  , xmax :: a
  , ymin :: a
  , ymax :: a
  }

lowerLeft :: Box a -> (a,a)
lowerLeft box = (xmin box, ymin box)

upperRight :: Box a -> (a,a)
upperRight box = (xmax box, ymax box)


-- clockwise
boxCorners :: Box a -> ((a,a),(a,a),(a,a),(a,a))
boxCorners (Box {xmin = x1, xmax = x2, ymin = y1, ymax = y2}) =
  ((x1,y1), (x1,y2), (x2,y2), (x2,y1))

-- clockwise
boxSides :: Box a -> (SegmentEquation a, SegmentEquation a, SegmentEquation a, SegmentEquation a)
boxSides box =
  let (ll,ul,ur,lr) = boxCorners box in
  let s = seEndpoints in
  (s ll ul, s ul ur, s ur lr, s lr ll)


{-------------}
{- Instances -}
{-------------}

{- IsSolutionOf -}

instance (Ord a, Num a) => EquationIn2Var a (Box a) where
  isSolutionOf2 p box =
    let (s1,s2,s3,s4) = boxSides box in
    or
      [ isSolutionOf2 p s1
      , isSolutionOf2 p s2
      , isSolutionOf2 p s3
      , isSolutionOf2 p s4
      ]


{----------------}
{- Constructors -}
{----------------}

-- safe constructor
makeBox :: (Ord a) => (a,a) -> (a,a) -> Maybe (Box a)
makeBox p@(x1,y1) q@(x2,y2) =
  if (x1 < x2) && (y1 < y2)
    then Just $ Box { xmin = x1, xmax = x2, ymin = y1, ymax = y2 }
    else Nothing


boundingBox :: (Ord a, Num a) => a -> [(a,a)] -> Box a
boundingBox margin ps =
  let
    xMin = (minimum $ map fst ps) - margin
    yMin = (minimum $ map snd ps) - margin

    xMax = (maximum $ map fst ps) + margin
    yMax = (maximum $ map snd ps) + margin

    Just box = makeBox (xMin,yMin) (xMax,yMax)
  in box


isInsideBox :: (Ord a, Num a) => (a,a) -> Box a -> Bool
isInsideBox (x,y) box = and
  [ xmin box < x && x < xmax box
  , ymin box < y && y < ymax box
  ]
