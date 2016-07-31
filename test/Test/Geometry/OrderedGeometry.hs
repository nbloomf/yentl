module Test.Geometry.OrderedGeometry (
  testOrderedGeometry
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Arb
import Test.Combinator
import Test.Verify

import Yentl.Geo
import Yentl.Geometry.IncidenceGeometry
import Yentl.Geometry.OrderedGeometry


testOrderedGeometry :: (Show t, ArbOrderedGeometry t)
  => String -> t -> TestTree
testOrderedGeometry name x = testGroup ("ordered geometry: " ++ name)
  [ testOrderedGeometryProperties name x
  , testOrderedGeometryTheorems name x
  ]



{--------------}
{- Properties -}
{--------------}

testOrderedGeometryProperties
  :: (Show t, ArbOrderedGeometry t) => String -> t -> TestTree
testOrderedGeometryProperties name x =
  testGroup ("ordered geometry properties: " ++ name)

  {- betweenness is symmetric -}
  [ testProperty
      "Bet(a,b,c) ==> Bet(c,b,a)" $
      forAll (between3 x) $ \(a,b,c) -> verify
        ( If
            [ isBetween b (a,c)
            ]
        , Then
            [ isBetween b (c,a)
            ]
        )


  {- betweenness trichotomy: uniqueness -}
  , testProperty
      "Bet(a,b,c) ==> not Bet(b,a,c) and not Bet(a,c,b)" $
      forAll (between3 x) $ \(a,b,c) -> verify
        ( If
            [ isBetween b (a,c)
            ]
        , Then
            [ not $ isBetween a (b,c)
            , not $ isBetween c (a,b)
            ]
        )


  {- betweenness trichotomy: existence -}
  , testProperty
      "Col(a,b,c) ==> Bet(a,b,c) or Bet(b,a,c) or Bet(a,c,b)" $
      forAll (collinear3 x) $ \(a,b,c) -> verify
        ( If
            [ areCollinear a b c
            ]
        , Then
            [ or
                [ isBetween a (b,c)
                , isBetween b (a,c)
                , isBetween c (a,b)
                ]
            ]
        )


  {- interpolated points are in order -}
  , testProperty
      "interpolated points are in order" $
      forAll (arbDistinctPoints2 x) $ \(u,v) -> verify
        ( If
            [ u /= v
            ]
        , Then
            [ let Just (a,b,c) = interpolate u v in
              and
                [ isBetween u (a,b)
                , isBetween b (u,v)
                , isBetween v (b,c)
                ]
            ]
        )
  ]



{------------}
{- Theorems -}
{------------}

testOrderedGeometryTheorems
  :: (Show t, ArbOrderedGeometry t) => String -> t -> TestTree
testOrderedGeometryTheorems name x =
  testGroup ("ordered geometry theorems: " ++ name)
  [
  ]
