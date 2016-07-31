module Test.Algebra.Equation.LinearForm (
  testLinearForm
) where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Combinator
import Test.Algebra.Gen

import Yentl.Algebra.Classes
import Yentl.Algebra.Number.Slope
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.System2
import Yentl.Algebra.Equation.LinearForm
import Yentl.Algebra.Equation.System



{---------------}
{- Linear Form -}
{---------------}

testLinearForm :: (Arbitrary a, Show a, Eq a, Num a, Fractional a)
  => a -> String -> TestTree
testLinearForm x name = testGroup ("Linear Form (" ++ name ++ ")")
  [ testProperty "vert. line x-int equality" $
      let
        test c =
          let Just ell = lfStandardForm 1 0 (-c) in
          let Just w = xIntercept ell in
          w == c
      in
        forAll
          (arb x)
          test


  , testProperty "x-int of nonhor. line is solution" $
      let
        test ell =
          let Just c = xIntercept ell in
          isSolutionOf2 (c,0) ell
      in
        forAll
        (arbLinearForm x `suchThat` (\ell -> not $ lfIsHorizontal ell))
        test


  , testProperty "line(p,q) == line(q,p)" $
      let
        test (p,q) =
          let Just ell1 = lfPointPoint p q in
          let Just ell2 = lfPointPoint q p in
          ell1 == ell2
      in
        forAll
          (diff2 $ spawn2 $ arb x)
          test

 
  , testProperty "p,q are solutions of line(p,q)" $
      let
        test (p,q) =
          let Just ell = lfPointPoint p q in
          isSolutionOf2 p ell && isSolutionOf2 q ell
      in
        forAll
          (diff2 $ spawn2 $ arb x)
          test


  , testProperty "line(p,q) is perp to perp. bis. of p & q" $
      let
        test (p,q) =
          let Just ell1 = lfPointPoint p q in
          let Just ell2 = lfPerpBisector p q in
          lfPerpendicular ell1 ell2
      in
        forAll
          (diff2 $ spawn2 $ arb x)
          test


  , testProperty "foot(p,q) and p.b.(p,q) are parallel" $
      let
        test (p,q) =
          let Just ell1 = lfPointFoot p q in
          let Just ell2 = lfPerpBisector p q in
          lfParallel ell1 ell2
      in
        forAll
          (diff2 $ spawn2 $ arb x)
          test


  , testProperty "if p,q are dist. sols of ell, then ell == line(p,q)" $
      let
        test ell =
          let (p,q) = lfTwoSolutionsOf ell in
          let Just ell2 = lfPointPoint p q in
          ell == ell2
      in
        forAll
          (arbLinearForm x)
          test


  , testProperty "3 noncol. pts. generate linear system with one sol." $
      let
        test (p,q,r) =
          let Just ell1 = lfPointPoint p q in
          let Just ell2 = lfPointPoint r p in
          let Just w = solve2var1sol (ell1, ell2) in
          w == p
      in
        forAll
          ((diff3 $ spawn2 $ arb x) `suchThat` (not . sameSlope))
          test
  ]
