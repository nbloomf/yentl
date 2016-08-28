module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.QuickCheck

import Yentl
import Yentl.Algebra
import Yentl.Model

import Test.Algebra
import Test.Arb
import Test.Combinator
import Test.Geometry
import Test.Model.CartesianPlane
import Test.Model.PoincarePlane
import Test.Model.PoincareDisc



main :: IO ()
main = defaultMain $ testGroup "testing"
  [ --testAlgebra (0::Rational) "Rational"
--  , testAlgebra (0::ConReal)  "ConReal"

--  , testLinearForm       (0::ConReal) "ConReal"
--  , testSegmentEquation  (0::ConReal) "ConReal"
--  , testCircleEquation   (0::ConReal) "ConReal"
--  , testMinorArcEquation (0::ConReal) "ConReal"

--  , testAlgPoint
   testPoincareDisc (originPoincareDisc :: PoincareDisc ConReal)
--  , testHypPoint
  ]



-- AlgPoint model
testAlgPoint :: TestTree
testAlgPoint = testGroup "Cartesian Plane Model"
  [ testIncidenceGeometry  n z
  , testOrderedGeometry    n z
  , testCongruenceGeometry n z
  , testPlaneGeometry      n z
  ]
  where
    n = "AlgPoint"
    z = P zero



-- HypPoint model
testHypPoint :: TestTree
testHypPoint = testGroup "Poincare Plane Model"
  [ testIncidenceGeometry  n z
  , testOrderedGeometry    n z
  , testCongruenceGeometry n z
  , testPlaneGeometry      n z
  ]
  where
    n = "HypPoint"
    z = H zero



testPoincareDisc :: (ArbIncidenceGeometry a, ArbOrderedGeometry a, ArbCongruenceGeometry a, ArbPlaneGeometry a) => a -> TestTree
testPoincareDisc x = testGroup "Poincare Disc Model"
  [ testIncidenceGeometry  n x
  , testOrderedGeometry    n x
  , testCongruenceGeometry n x
  , testPlaneGeometry      n x
  ]
  where n = "PoincareDisc"
