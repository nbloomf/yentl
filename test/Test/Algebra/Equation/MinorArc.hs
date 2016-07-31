module Test.Algebra.Equation.MinorArc (
  testMinorArcEquation
) where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Combinator
import Test.Algebra.Gen

import Yentl.Algebra.Classes
import Yentl.Algebra.Equation.Curve
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.MinorArc


testMinorArcEquation :: (Arbitrary a, Show a, Ord a, Num a, Fractional a, SquareRoot a)
  => a -> String -> TestTree
testMinorArcEquation x name = testGroup ("Minor Arc Equation (" ++ name ++ ")")
  [ testProperty "endpoints are solutions" $
      let
        test arc =
          let (a,b) = maCoords arc in
          (isSolutionOf2 a arc) && (isSolutionOf2 b arc)
      in
        forAll
          (arbMinorArcEquation x)
          test


  , testProperty "point along is on" $
      let
        test (arc,t) = isSolutionOf2 (along t arc) arc
      in
        forAll
          (tuple2 (arbMinorArcEquation x) arbitrary)
          test
  ]
