module Test.Model.CartesianPlane () where

import Test.QuickCheck

import Test.Arb
import Test.Combinator

import qualified Yentl.Algebra as A
import Yentl.Geometry
import Yentl.Model.CartesianPlane



instance Arbitrary CartesianPlane where
  arbitrary = fmap P arbitrary



{----------------------}
{- Incidence Geometry -}
{----------------------}

instance ArbIncidenceGeometry CartesianPlane where
  arbPointDistinctFrom x = do
    y <- arb x `suchThat` (/= x)
    return y


  pointOnLine (P a) (P b) = do
    t <- arbitrary `suchThat` (\u -> u /= 0 && u /= 1)
    return $ P (A.interpolate t a b)



{--------------------}
{- Ordered Geometry -}
{--------------------}

instance ArbOrderedGeometry CartesianPlane where
  between3 x = do
    (P a, P b) <- diff2 $ arb x
    (t1,t2,t3) <- sorted3 $ arb 0
    let c1 = A.interpolate t1 a b
    let c2 = A.interpolate t2 a b
    let c3 = A.interpolate t3 a b
    return (P c1, P c2, P c3)


  between4 x = do
    (P a, P b) <- diff2 $ arb x
    (t1,t2,t3,t4) <- sorted4 $ arb 0
    let c1 = A.interpolate t1 a b
    let c2 = A.interpolate t2 a b
    let c3 = A.interpolate t3 a b
    let c4 = A.interpolate t4 a b
    return (P c1, P c2, P c3, P c4)



instance ArbCongruenceGeometry CartesianPlane where



instance ArbPlaneGeometry CartesianPlane where
