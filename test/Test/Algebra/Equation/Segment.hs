module Test.Algebra.Equation.Segment (
  testSegmentEquation
) where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Combinator
import Test.Algebra.Gen

import Yentl.Algebra.Classes
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.Curve
import Yentl.Algebra.Equation.Segment


testSegmentEquation :: (Arbitrary a, Show a, Ord a, Num a, Fractional a, SquareRoot a)
  => a -> String -> TestTree
testSegmentEquation x name = testGroup ("Segment Equation (" ++ name ++ ")")
  [ testProperty "endpoints are solutions" $
      let
        test seg =
          let (p,q) = seCoords seg in
          (isSolutionOf2 p seg) && (isSolutionOf2 q seg)
      in
        forAll
          (arbSegmentEquation x)
          test


  , testProperty "midpoint is solution" $
      let
        test seg =
          let (p,q) = seCoords seg in
          isSolutionOf2 (midpoint p q) seg
      in
        forAll
          (arbSegmentEquation x)
          test


  , testProperty "point along is on" $
      let
        test (t,seg) = isSolutionOf2 (along t seg) seg
      in
        forAll
          (tuple2 (arb x) (arbNontrivialSegmentEquation x))
          test
  ]
