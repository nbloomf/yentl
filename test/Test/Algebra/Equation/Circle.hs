module Test.Algebra.Equation.Circle (
  testCircleEquation
) where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Combinator
import Test.Algebra.Gen

import Yentl.Algebra.Classes
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.Circle


{-------------------------}
{- Circle Equation Tests -}
{-------------------------}

testCircleEquation :: (Arbitrary a, Show a, Ord a, Num a, Fractional a, SquareRoot a)
  => a -> String -> TestTree
testCircleEquation x name = testGroup ("Circle Equation (" ++ name ++ ")")

  {- endpoints of diameter are solutions -}
  [ testProperty "diameter endpoints are solutions" $
      let
        test (p,q) =
          let circ = ceDiameter p q in
          (isSolutionOf2 p circ) && (isSolutionOf2 q circ)
      in
        forAll
          (spawn2 $ spawn2 $ arb x)
          test


  {- point inversion identity -}
  , testProperty "point inversion identity: oa*ob == r^2" $
      let
        test (r,(o,p)) =
          let circ = ceCenterRadius o r in
          let Just q = ceInvertPoint circ p in
          ((dotSq $ p -. o) * (dotSq $ q -. o)) == r^4
      in
        forAll
          (tuple2
            (arb x `suchThat` (/= 0))
            (diff2 $ spawn2 $ arb x))
          test


  {- center radius constructor -}
  , testProperty "center radius constructor" $
      let
        test circ1 =
          let o = ceCenterOf circ1 in
          let r = ceRadiusOf circ1 in
          let circ2 = ceCenterRadius o r in
          circ1 == circ2
      in
        forAll
          (arbCircleEquation x)
          test
  ]
