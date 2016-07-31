module Test.Model.PoincarePlane () where


import Test.QuickCheck

import Test.Arb
import Test.Combinator

import qualified Yentl.Algebra as A
import Yentl.Geometry
import Yentl.Model.PoincarePlane



instance Arbitrary PoincarePlane where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary `suchThat` (\z -> z > 0)
    return $ H (x, y)


{----------------------}
{- Incidence Geometry -}
{----------------------}

instance ArbIncidenceGeometry PoincarePlane where
  arbPointDistinctFrom x = do
    toggle <- arbitrary
    let H (a,b) = x
    if toggle
      then do
        c <- arbitrary `suchThat` (\z -> z > 0 && z /= b) 
        return (H (a,c))
      else do
        (d,c) <- arbitrary `suchThat` (\(u,v) -> (u,v) /= (a,b) && v > 0)
        return (H (d,c))


  pointOnLine (H a) (H b) = do
    t <- arbitrary `suchThat` (\u -> u /= 0 && u /= 1)
    return $ H (interpolateH t a b)



{--------------------}
{- Ordered Geometry -}
{--------------------}

instance ArbOrderedGeometry PoincarePlane where
  between3 x = do
    (H a, H b) <- diff2 $ arb x
    (t1,t2,t3) <- sorted3 $ arb 0
    let c1 = interpolateH t1 a b
    let c2 = interpolateH t2 a b
    let c3 = interpolateH t3 a b
    return (H c1, H c2, H c3)

  between4 x = do
    (H a, H b) <- diff2 $ arb x
    (t1,t2,t3,t4) <- sorted4 $ arb 0
    let c1 = interpolateH t1 a b
    let c2 = interpolateH t2 a b
    let c3 = interpolateH t3 a b
    let c4 = interpolateH t4 a b
    return (H c1, H c2, H c3, H c4)



{-----------------------}
{- Congruence Geometry -}
{-----------------------}

instance ArbCongruenceGeometry PoincarePlane where



{------------------}
{- Plane Geometry -}
{------------------}

instance ArbPlaneGeometry PoincarePlane where
