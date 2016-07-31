module Test.Geometry.PlaneGeometry (
  testPlaneGeometry
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Arb
import Test.Combinator
import Test.Verify

import Yentl.Geo
import Yentl.Geometry.OrderedGeometry
import Yentl.Geometry.CongruenceGeometry
import Yentl.Geometry.PlaneGeometry
import Yentl.Theorems.PlaneGeometry



testPlaneGeometry :: (Show t, ArbPlaneGeometry t)
  => String -> t -> TestTree
testPlaneGeometry name x = testGroup ("plane geometry: " ++ name)
  [ testPlaneGeometryProperties name x
  , testPlaneGeometryTheorems name x
  ]



{--------------}
{- Properties -}
{--------------}

testPlaneGeometryProperties
  :: (Show t, ArbPlaneGeometry t) => String -> t -> TestTree
testPlaneGeometryProperties name x =
  testGroup ("plane geometry properties: " ++ name)

  -- circle ray cut gives congruent segment
  [ testProperty
      "circle ray cut congruence" $
      forAll (arbPointsUniqueFirst3 x) $ \(o,a,b) -> verify
        ( If
          [ a /= o
          , b /= o
          ]
      , Then
          [ let Just c = cutCircleRay (o,a) b in
            segmentCongruence (o,a) (o,c)
          ]
      )


{-
  -- if [abcd] then circles with diameters ac and bd intersect in two pts
  , testProperty
      "[abcd] ==> Diam(a,c) cap Diam(b,d) is a doubleton" $
      forAll (between4 x) $ \(a,b,c,d) -> verify
        ( If 
          [ areInOrder [a,b,c,d]
          ]
      , Then
          [ let Just (p,q) = cutCircles a b c d in
            p /= q
          ]
      )
-}
  ]



{------------}
{- Theorems -}
{------------}

testPlaneGeometryTheorems
  :: (Show t, ArbPlaneGeometry t) => String -> t -> TestTree
testPlaneGeometryTheorems name x =
  testGroup ("plane geometry theorems: " ++ name)
  [ testProperty
      "antipode congruence" $
      forAll (arbDistinctPoints2 x)
        (\(a,b) -> isValid $ thmAntipodeCongruence a b)

  , testProperty
      "antipode between" $
      forAll (arbDistinctPoints2 x)
        (\(a,b) -> isValid $ thmAntipodeBetween a b)

  , testProperty
      "equilateral points congruence" $
      forAll (arbDistinctPoints2 x)
        (\(a,b) -> isValid $ thmEquilateralPointsCongruence a b)

  , testProperty
      "equilateral points opposite sides" $
      forAll (arbDistinctPoints2 x)
        (\(a,b) -> isValid $ thmEquilateralPointsOppositeSides a b)

  , testProperty
      "copied segment is congruent" $
      forAll (tuple2 (arbSegment x) (arbRay x))
        (\(s,r) -> isValid $ thmCopySegmentToRayCongruence s r)
  ]
