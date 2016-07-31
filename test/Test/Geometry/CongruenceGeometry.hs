module Test.Geometry.CongruenceGeometry (
  testCongruenceGeometry
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Arb
import Test.Combinator

import Yentl.Geo
import Yentl.Geometry.IncidenceGeometry
import Yentl.Geometry.OrderedGeometry
import Yentl.Geometry.CongruenceGeometry


testCongruenceGeometry :: (Show t, ArbCongruenceGeometry t)
  => String -> t -> TestTree
testCongruenceGeometry name x = testGroup ("congruence geometry: " ++ name)
  [ testCongruenceGeometryProperties name x
  , testCongruenceGeometryTheorems name x
  ]


testCongruenceGeometryProperties
  :: (Show t, ArbCongruenceGeometry t) => String -> t -> TestTree
testCongruenceGeometryProperties name x =
  testGroup ("congruence geometry properties: " ++ name)
  [
  ]


testCongruenceGeometryTheorems
  :: (Show t, ArbCongruenceGeometry t) => String -> t -> TestTree
testCongruenceGeometryTheorems name x =
  testGroup ("congruence geometry theorems: " ++ name)
  [
  ]
