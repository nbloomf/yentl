module Test.Algebra.Classes (
  testFieldProps,
  testModuleProps,
  testVectorProps,
  testSquareMatrixProps,
  testMatMultProps
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Laws

import Yentl.Algebra.Classes


{--------------------}
{- Field Properties -}
{--------------------}

testFieldProps :: (Arbitrary t, Show t, Eq t, Num t)
  => String -> t -> TestTree
testFieldProps name x = testGroup ("field properties: " ++ name)
  [ testProperty "(a+b)+c == a+(b+c)"    $ testAssoc (xPlus x)
  , testProperty "a+b == b+a"            $ testCommu (xPlus x)
  , testProperty "0+a == a"              $ testNeutL (xPlus x) 0
  , testProperty "a+0 == a"              $ testNeutR (xPlus x) 0
  , testProperty "a+(-a) == 0 == (-a)+a" $ testNeg   (xPlus x) negate 0
  , testProperty "(a*b)*c == a*(b*c)"    $ testAssoc (xTimes x)
  , testProperty "a*b == b*a"            $ testCommu (xTimes x)
  , testProperty "1*a == a"              $ testNeutL (xTimes x) 1
  , testProperty "a*1 == a"              $ testNeutR (xTimes x) 1
  , testProperty "a*(b+c) == a*b + a*c"  $ testDistL (xTimes x) (xPlus x) (xPlus x)
  , testProperty "(a+b)*c == a*c + b*c"  $ testDistR (xTimes x) (xPlus x) (xPlus x)
  , testProperty "-0 == 0"               $ testValue (xNegate x) 0 0
  , testProperty "-(-a) == a"            $ testIdemp (xNegate x)
  , testProperty "(-1)*a == -a"          $ testNegOneL (xTimes x) negate negate 1
  , testProperty "a*(-1) == -a"          $ testNegOneR (xTimes x) negate negate 1
  , testProperty "(-a)*b == -(a*b)"      $ testNegProdL (xTimes x) negate negate
  , testProperty "a*(-b) == -(a*b)"      $ testNegProdR (xTimes x) negate negate
  ]
  where
    xPlus, xTimes :: (Num t) => t -> t -> t -> t
    xPlus _  = (+)
    xTimes _ = (*)

    xNegate :: (Num t) => t -> t -> t
    xNegate _ = negate



{---------------------}
{- Module Properties -}
{---------------------}

testModuleProps :: (Show r, Eq r, Num r, Arbitrary r, Arbitrary t, Show t, Eq t, Module r t)
  => String -> t -> TestTree
testModuleProps name x = testGroup ("module properties: " ++ name)
  [ testProperty "(a+b)+c == a+(b+c)"    $ testAssoc (xPlus x)
  , testProperty "a+b == b+a"            $ testCommu (xPlus x)
  , testProperty "0+a == a"              $ testNeutL (xPlus x) zero
  , testProperty "a+0 == a"              $ testNeutR (xPlus x) zero
  , testProperty "a+(-a) == 0 == (-a)+a" $ testNeg   (xPlus x) minus zero
  , testProperty "x*(a+b) == x*a + x*b"  $ testDistL (xScale x) (xPlus x) (xPlus x)
  , testProperty "(x+y)*a == x*a + y*a"  $ testDistR (xScale x) (+) (xPlus x)
  , testProperty "-0 == 0"               $ testValue (xMinus x) zero zero
  , testProperty "-(-a) == a"            $ testIdemp (xMinus x)
  , testProperty "(-1)*a == -a"          $ testNegOneL (xScale x) negate minus 1
  , testProperty "(-x)*a == -(x*b)"      $ testNegProdL (xScale x) negate minus
  , testProperty "x*(-a) == -(x*b)"      $ testNegProdR (xScale x) minus minus
  ]
  where
    xPlus :: (Module r t) => t -> t -> t -> t
    xPlus _ = (+.)

    xScale :: (Module r t) => t -> r -> t -> t
    xScale _ = (@.)

    xMinus :: (Module r t) => t -> t -> t
    xMinus _ = minus



{---------------------}
{- Vector Properties -}
{---------------------}

testVectorProps :: (Module r t, Eq r, Num r, Show r, Arbitrary r, Arbitrary t, Show t, Eq t, Vector r t) => String -> t -> TestTree
testVectorProps name x = testGroup ("vector properties: " ++ name)
  [ testProperty "v·w == w·v"           $ testCommu (xDot x)
  , testProperty "u·(v+w) == u·v + u·w" $ testDistL (xDot x) (xPlus x) (+)
  , testProperty "(u+v)·w == u·w + v·w" $ testDistR (xDot x) (xPlus x) (+)
  , testProperty "(-v)·w == -(v·w)"     $ testNegProdL (xDot x) (xMinus x) negate
  , testProperty "v·(-w) == -(v·w)"     $ testNegProdR (xDot x) (xMinus x) negate
  ]
  where
    xPlus :: (Module r t) => t -> t -> t -> t
    xPlus _ = (+.)

    xMinus :: (Module r t) => t -> t -> t
    xMinus _ = minus

    xDot :: (Vector r t) => t -> t -> t -> r
    xDot _ = dot



{----------------------------}
{- Square Matrix Properties -}
{----------------------------}

testSquareMatrixProps :: (Module r t, Ord r, Num r, Arbitrary t, Show t, Eq t, SquareMatrix r t) => String -> t -> TestTree
testSquareMatrixProps name x = testGroup ("square matrix properties: " ++ name)
  [ testProperty "a*(b+c) == a*b + a*c"      $ testDistL (xTimes x) (xPlus x) (xPlus x)
  , testProperty "(a+b)*c == a*c + b*c"      $ testDistR (xTimes x) (xPlus x) (xPlus x)
  , testProperty "(a*b)*c == a*(b*c)"        $ testAssoc (xTimes x)
  , testProperty "1*a == a"                  $ testNeutL (xTimes x) one
  , testProperty "a*1 == a"                  $ testNeutR (xTimes x) one
  , testProperty "(-a)*b == -(a*b)"          $ testNegProdL (xTimes x) minus minus
  , testProperty "a*(-b) == -(a*b)"          $ testNegProdR (xTimes x) minus minus
  , testProperty "det(I) == 1"               $ testValue (xDet x) one 1
  , testProperty "det(a*b) == det(a)*det(b)" $ testHom (xDet x) (xTimes x) (*)
  ]
  where
    xPlus :: (Module r t) => t -> t -> t -> t
    xPlus _ = (+.)

    xTimes :: (SquareMatrix r t) => t -> t -> t -> t
    xTimes _ = (*.)

    xDet :: (SquareMatrix r t) => t -> t -> r
    xDet _ = det



{------------------------------------}
{- Matrix Multiplication Properties -}
{------------------------------------}

testMatMultProps :: (Module r u, Module r t, Num r, Eq r, Show t, Show u, Arbitrary t, Arbitrary u, Eq u, MatMult t u, SquareMatrix r t) => String -> String -> t -> u -> TestTree
testMatMultProps name1 name2 x y = testGroup
  ("matrix multiplication properties: " ++ name1 ++ " " ++ name2)
  [ testProperty "a*(b+c) == a*b + a*c"      $ testDistL (xTimes x y) (xPlus y) (xPlus y)
  , testProperty "(a+b)*c == a*c + b*c"      $ testDistR (xTimes x y) (xPlus x) (xPlus y)
  , testProperty "I*a == a"                  $ testNeutL (xTimes x y) one
  , testProperty "(-a)*b == -(a*b)"          $ testNegProdL (xTimes x y) (xMinus x) (xMinus y)
  , testProperty "a*(-b) == -(a*b)"          $ testNegProdR (xTimes x y) (xMinus y) (xMinus y)
  ]
  where
    xPlus :: (Module r v) => v -> v -> v -> v
    xPlus _ = (+.)

    xMinus :: (Module r v) => v -> v -> v
    xMinus _ = minus

    xTimes :: (MatMult v w) => v -> w -> v -> w -> w
    xTimes _ _ = (%.)
