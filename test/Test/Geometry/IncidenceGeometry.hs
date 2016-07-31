module Test.Geometry.IncidenceGeometry (
  testIncidenceGeometry
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Arb
import Test.Combinator
import Test.Verify

import Yentl.Geo
import Yentl.Geometry.IncidenceGeometry
import Yentl.Theorems.IncidenceGeometry


testIncidenceGeometry :: (Show t, ArbIncidenceGeometry t)
  => String -> t -> TestTree
testIncidenceGeometry name x = testGroup ("incidence geometry: " ++ name)
  [ testIncidenceGeometryProperties name x
  , testIncidenceGeometryTheorems name x
  ]



{--------------}
{- Properties -}
{--------------}

testIncidenceGeometryProperties :: (Show t, ArbIncidenceGeometry t) => String -> t -> TestTree
testIncidenceGeometryProperties name x =
  testGroup ("incidence geometry properties: " ++ name)

  {- collinearity is permutable -}
  [ testProperty
      "Col(a,b,c) ==> Col(sa,sb,sc) for any permutation s" $
      forAll (collinear3 x) $ \(a,b,c) -> verify
        ( If 
            [ areCollinear a b c
            ]
        , Then
            [ areCollinear a c b
            , areCollinear b a c
            ]
        )


  {- any two distinct points are collinear -}
  , testProperty
      "Col(a,a,b) && Col(a,b,a) && Col(b,a,a)" $
      forAll (diff2 $ arb x) $ \(a,b) -> verify
        ( If
            [ a /= b
            ]
        , Then
            [ areCollinear a a b
            , areCollinear a b a
            , areCollinear b a a
            ]
        )


  {- collinear points lie on the lines they generate -}
  , testProperty
      "Col(a,b,c) ==> a on L(b,c), b on L(a,c), c on L(a,b)" $
      forAll (collinear3 x) $ \(a,b,c) -> verify
        ( If
            [ a /= b
            , a /= c
            , b /= c
            , areCollinear a b c
            ]
        , Then
            [ let Just ell = lineMaybe b c in liesOn a ell
            , let Just ell = lineMaybe a c in liesOn b ell
            , let Just ell = lineMaybe a b in liesOn c ell
            ]
        )


  {- points lie on the line they generate -}
  , testProperty
      "a /= b ==> a & b on L(a,b)" $
      forAll (diff2 $ arb x) $ \(a,b) -> verify
        ( If
            [ a /= b
            ]
        , Then
            [ let Just ell = lineMaybe a b in
              liesOn a ell && liesOn b ell
            ]
        )


  {- noncommutative triples generate incident lines -}
  , testProperty
      "NonCol(a,b,c) ==> L(a,b) and L(b,c) are incident" $
      forAll (noncollinear3 x) $ \(a,b,c) -> verify
        ( If
            [ not $ areCollinear a b c
            ]
        , Then
            [ intersectLines (a,b) (a,c) == Just a
            , intersectLines (a,b) (b,c) == Just b
            , intersectLines (a,c) (b,c) == Just c
            ]
        )


  {- line constructor is commutative -}
  , testProperty
      "a /= b ==> line(a,b) == line(b,a)" $
      forAll (diff2 $ arb x) $ \(a,b) -> verify
        ( If
            [ a /= b
            ]
        , Then
            [ let
                Just ell1 = lineMaybe a b
                Just ell2 = lineMaybe b a
              in ell1 == ell2
            ]
        )
  ]



{------------}
{- Theorems -}
{------------}

testIncidenceGeometryTheorems
  :: (Show t, ArbIncidenceGeometry t) => String -> t -> TestTree
testIncidenceGeometryTheorems name x =
  testGroup ("incidence geometry theorems: " ++ name)
  [ testProperty
      "line trichotomy" $
       forAll (spawn2 $ arbLine x)
         (uncurry thmLineTrichotomy)
  ]
