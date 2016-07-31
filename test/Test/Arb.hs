{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Test.Arb (
  ArbIncidenceGeometry,
  -- primitive generators
  arbPointDistinctFrom,
  arbLine, pointOnLine, collinear3, noncollinear3,

  -- derived generators
  arbDistinctPoints2, arbPointsUniqueFirst3,

  ArbOrderedGeometry,
  between3, between4, arbSegment, arbRay,

  ArbCongruenceGeometry,
  arbCircle, arbIncidentCircles2,

  ArbPlaneGeometry
) where


import Data.List (sort)

import Test.QuickCheck

import Test.Combinator
import Test.Algebra

import Yentl.Geo
import Yentl.Model
import Yentl.Geometry



{------------------------}
{- ArbIncidenceGeometry -}
{------------------------}

class (Eq t, Show t, Arbitrary t, IncidenceGeometry t)
  => ArbIncidenceGeometry t where
  -- point distinct from a given point
  arbPointDistinctFrom :: t -> Gen t

  -- arbitrary (distinct) point on given line
  pointOnLine :: t -> t -> Gen t

  -- 3 distinct collinear points
  collinear3 :: t -> Gen (t,t,t)
  collinear3 x = do
    (a,b) <- diff2 $ arb x
    c <- (pointOnLine a b) `suchThat` (\m -> m /= a && m /= b)
    return (a,b,c)

  -- 3 distinct noncollinear points
  noncollinear3 :: t -> Gen (t,t,t)
  noncollinear3 x =
    (diff3 $ arb x) `suchThat`
      (\(a,b,c) -> not $ areCollinear a b c)

  -- arbitrary line
  arbLine :: t -> Gen (Line t)
  arbLine x = do
    (p,q) <- arbDistinctPoints2 x
    let Just ell = lineMaybe p q
    return ell


{- Derived -}

-- two distinct points
arbDistinctPoints2 :: (ArbIncidenceGeometry t) => t -> Gen (t,t)
arbDistinctPoints2 x = do
  a <- arb x
  b <- arbPointDistinctFrom a
  return (a,b)

-- (a,b,c) where b,c are distinct from a, maybe not from each other.
arbPointsUniqueFirst3 :: (ArbIncidenceGeometry t) => t -> Gen (t,t,t)
arbPointsUniqueFirst3 x = do
  a <- arb x
  toggle <- arb (0 :: Int)
  if toggle `mod` 10 == 0
    then do
      b <- arbPointDistinctFrom a
      return (a,b,b)
    else do
      b <- arbPointDistinctFrom a
      c <- arbPointDistinctFrom a `suchThat` (/= b)
      return (a,b,c)



{----------------------}
{- ArbOrderedGeometry -}
{----------------------}

class (ArbIncidenceGeometry t, OrderedGeometry t)
  => ArbOrderedGeometry t where
  -- 3 collinear points in order
  between3 :: t -> Gen (t,t,t)

  -- 4 collinear points in order
  between4 :: t -> Gen (t,t,t,t)


{- Derived -}

-- arbitrary segment
arbSegment :: (ArbOrderedGeometry t) => t -> Gen (Segment t)
arbSegment x = do
  (p,q) <- arbDistinctPoints2 x
  let Just ell = segmentMaybe p q
  return ell


-- arbitrary ray
arbRay :: (ArbOrderedGeometry t) => t -> Gen (Ray t)
arbRay x = do
  (p,q) <- arbDistinctPoints2 x
  let Just ell = rayMaybe p q
  return ell



{-------------------------}
{- ArbCongruenceGeometry -}
{-------------------------}

class (ArbOrderedGeometry t, CongruenceGeometry t)
  => ArbCongruenceGeometry t where
  -- arbitrary circle
  arbCircle :: t -> Gen (Circle t)
  arbCircle x = do
    (p,q) <- arbDistinctPoints2 x
    let Just circ = circleMaybe p q
    return circ

  -- arbitrary pair of incident circles
  arbIncidentCircles2 :: t -> Gen (Circle t, Circle t)
  arbIncidentCircles2 x = do
    (a,x,b,y) <- between4 x
    let Just c1 = circleMaybe a b
    let Just c2 = circleMaybe y x
    return (c1,c2)
    


{--------------------}
{- ArbPlaneGeometry -}
{--------------------}

class (ArbCongruenceGeometry t, PlaneGeometry t)
  => ArbPlaneGeometry t where
