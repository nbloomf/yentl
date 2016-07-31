module Test.Model.PoincareDisc () where


import Test.QuickCheck

import Test.Arb
import Test.Combinator

import qualified Yentl.Algebra as A
import Yentl.Geometry
import Yentl.Model.PoincareDisc



instance (Arbitrary a, Show a, Ord a, Num a, Fractional a, A.SquareRoot a)
  => Arbitrary (PoincareDisc a) where
  arbitrary = do
    x <- arbInInterval (-1/2) (1/2)
    -- let Just r = A.root2 $ 1 - x^2
    y <- arbInInterval (-1/2) (1/2)
    let Just p = mkPoincareDisc (x,y)
    return p



{----------------------}
{- Incidence Geometry -}
{----------------------}


instance (Arbitrary a, Show a, Ord a, Num a, Fractional a, A.SquareRoot a)
  => ArbIncidenceGeometry (PoincareDisc a) where
  arbPointDistinctFrom p = arbitrary `suchThat` (/= p)


  pointOnLine r s = do
    let (a,b) = (unPD r, unPD s)
    case lineType a b of
      Nothing -> return r

      Just (TypeI m) -> do
        let Just (u,v) = idealPoints a b
        let seg = A.seEndpoints u v
        t <- arbitrary
        let Just p = mkPoincareDisc (A.along t seg)
        return p

      Just (TypeII o) -> do
        let Just (u,v) = idealPoints a b
        let Just arc = A.maCenterEndpoints o (u,v)
        t <- arbitrary
        let Just p = mkPoincareDisc (A.along t arc)
        return p



{--------------------}
{- Ordered Geometry -}
{--------------------}

instance (Arbitrary a, Show a, Ord a, Num a, Fractional a, A.SquareRoot a)
  => ArbOrderedGeometry (PoincareDisc a) where
  between3 x = do
    (a,b) <- diff2 $ arb x
    (t1,t2,t3) <- sorted3 $ arb 0
    let c1 = interpolatePD t1 a b
    let c2 = interpolatePD t2 a b
    let c3 = interpolatePD t3 a b
    return (c1,c2,c3)

  between4 x = do
    (a,b) <- diff2 $ arb x
    (t1,t2,t3,t4) <- sorted4 $ arb 0
    let c1 = interpolatePD t1 a b
    let c2 = interpolatePD t2 a b
    let c3 = interpolatePD t3 a b
    let c4 = interpolatePD t4 a b
    return (c1,c2,c3,c4)


{-----------------------}
{- Congruence Geometry -}
{-----------------------}

instance (Arbitrary a, Show a, Ord a, Num a, Fractional a, A.SquareRoot a)
  => ArbCongruenceGeometry (PoincareDisc a) where



{------------------}
{- Plane Geometry -}
{------------------}

instance (Arbitrary a, Show a, Ord a, Num a, Fractional a, A.SquareRoot a)
  => ArbPlaneGeometry (PoincareDisc a) where

