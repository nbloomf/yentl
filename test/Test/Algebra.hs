module Test.Algebra (
  testAlgebra,
  module Test.Algebra.Equation
) where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Combinator
import Test.Algebra.Classes
import Test.Algebra.Number
import Test.Algebra.Equation
import Test.Algebra.Data

import Yentl.Algebra



testAlgebra :: (Arbitrary r, Show r, Ord r, Num r, Fractional r)
  => r -> String -> TestTree
testAlgebra x name = testGroup ("testing " ++ name)
  [ testFieldProps name x
  , testPair x name
  , testMat2 x name
  , testMat3 x name
  , testMat4 x name
  ]
