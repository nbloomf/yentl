module Test.Algebra.Gen (
  -- Linear Form
  arbLinearForm,
  arbLinearFormSolution,

  -- Segment
  arbSegmentEquation,
  arbNontrivialSegmentEquation,

  -- Circle
  arbCircleEquation,
  arbNontrivialCircleEquation,

  -- Minor Arc
  arbMinorArcEquation
) where

import Test.QuickCheck

import Test.Combinator

import Yentl.Algebra


{--------------}
{- Generators -}
{--------------}

{- Linear Form -}

arbLinearForm :: (Arbitrary a, Eq a, Num a)
  => a -> Gen (LinearForm a)
arbLinearForm _ = do
  (a,b) <- arbitrary `suchThat` (\(u,v) -> u /= v)
  c <- arbitrary
  let Just ell = lfStandardForm a b c
  return ell


arbLinearFormSolution :: (Arbitrary a, Ord a, Num a, Fractional a)
  => LinearForm a -> Gen (a,a)
arbLinearFormSolution eq = do
  t <- arbitrary
  let p = along t eq
  let True = isSolutionOf2 p eq
  return p



{- Segment -}

arbSegmentEquation :: (Arbitrary a, Ord a, Num a)
  => a -> Gen (SegmentEquation a)
arbSegmentEquation _ = do
  p <- arbitrary
  toggle <- arbitrary :: Gen Int
  if toggle `mod` 10 == 0
    then do
      return $ seEndpoints p p
    else do
      q <- arbitrary `suchThat` (/= p)
      return $ seEndpoints p q


arbNontrivialSegmentEquation :: (Arbitrary a, Eq a, Num a)
  => a -> Gen (SegmentEquation a)
arbNontrivialSegmentEquation _ = do
  (p,q) <- diff2 arbitrary
  let True = p /= q
  return $ seEndpoints p q



{- Circle -}

arbCircleEquation :: (Arbitrary a, Ord a, Num a, Fractional a, SquareRoot a)
  => a -> Gen (CircleEquation a)
arbCircleEquation _ = do
  o <- arbitrary
  r <- arbitrary
  return $ ceCenterRadius o r


arbNontrivialCircleEquation :: (Arbitrary a, Ord a, Num a, Fractional a, SquareRoot a)
  => a -> Gen (CircleEquation a)
arbNontrivialCircleEquation _ = do
  o <- arbitrary
  r <- arbitrary `suchThat` (/= 0)
  return $ ceCenterRadius o r



{- Minor Arc -}

arbMinorArcEquation :: (Arbitrary a, Ord a, Num a, Fractional a, SquareRoot a)
  => a -> Gen (MinorArcEquation a)
arbMinorArcEquation _ = do
  (a,b) <- diff2 arbitrary
  let Just ell = lfPerpBisector a b
  t <- arbitrary `suchThat` (\k -> along k ell /= midpoint a b)
  let Just arc = maCenterEndpoints (along t ell) (a,b)
  return arc
