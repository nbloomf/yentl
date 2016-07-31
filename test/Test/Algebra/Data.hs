module Test.Algebra.Data (
  testPair,
  testMat2,
  testMat3,
  testMat4
) where

import Test.Tasty (TestTree, testGroup)
import Test.QuickCheck

import Test.Algebra.Classes

import Yentl.Algebra.Classes
import Yentl.Algebra.Data.Mat2
import Yentl.Algebra.Data.Mat3
import Yentl.Algebra.Data.Mat4


{---------------}
{- Test Suites -}
{---------------}

testPair :: (Arbitrary r, Show r, Num r, Eq r)
  => r -> String -> TestTree
testPair x str = testGroup name
  [ testModuleProps name (x,x)
  , testVectorProps name (x,x)
  ]
  where
    name = "(" ++ str ++ ", " ++ str ++ ")"


testMat2 :: (Arbitrary r, Show r, Num r, Fractional r, Ord r)
  => r -> String -> TestTree
testMat2 x str = testGroup name
  [ testModuleProps       name (m2 x)
  , testSquareMatrixProps name (m2 x)
  , testMatMultProps      name "(Vec2)" (m2 x) (x,x)
  ]
  where
    name = "Mat2 " ++ str

    m2 :: (Num r) => r -> Mat2 r
    m2 _ = zero


testMat3 :: (Arbitrary r, Show r, Num r, Fractional r, Ord r)
  => r -> String -> TestTree
testMat3 x str = testGroup name
  [ testModuleProps       name (m3 x)
  , testSquareMatrixProps name (m3 x)
  ]
  where
    name = "Mat3 " ++ str

    m3 :: (Num r) => r -> Mat3 r
    m3 _ = zero


testMat4 :: (Arbitrary r, Show r, Num r, Fractional r, Ord r)
  => r -> String -> TestTree
testMat4 x str = testGroup name
  [ testModuleProps       name (m4 x)
  , testSquareMatrixProps name (m4 x)
  ]
  where
    name = "Mat4 " ++ str

    m4 :: (Num r) => r -> Mat4 r
    m4 _ = zero



{-----------------------}
{- Arbitrary Instances -}
{-----------------------}

instance (Arbitrary a) => Arbitrary (Mat2 a) where
  arbitrary = do
    arbitrary >>= (return . fromRows2)

instance (Arbitrary a) => Arbitrary (Mat3 a) where
  arbitrary = do
    t <- arbitrary
    return $ fromRows3 t

instance (Arbitrary a) => Arbitrary (Mat4 a) where
  arbitrary = do
    t <- arbitrary
    return $ fromRows4 t
