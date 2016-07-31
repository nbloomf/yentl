{-# LANGUAGE FlexibleInstances #-}

module Yentl.Model.CartesianPlane (
  CartesianPlane(P), unP
) where


import qualified Yentl.Algebra as A
import Yentl.Geo
import Yentl.Geometry
import Yentl.View.Cartesian



{--------}
{- Type -}
{--------}

data CartesianPlane = P
  { unP :: (A.ConReal, A.ConReal)
  } deriving Eq


instance Show CartesianPlane where
  show = show . unP


instance Coords CartesianPlane where
  coords a b = return $ P { unP = (fromRational a, fromRational b) }



{----------------------}
{- Incidence Geometry -}
{----------------------}

instance IncidenceGeometry CartesianPlane where
  areCollinear (P x) (P y) (P z) =
    if x == y
      then x /= z
      else if y == z
        then True
        else
          let
            Just mxy = A.slopeBetween x y
            Just myz = A.slopeBetween y z
          in mxy == myz


  intersectLines (P x1, P y1) (P x2, P y2) = do
    form1 <- A.lfPointPoint x1 y1
    form2 <- A.lfPointPoint x2 y2
    fmap P $ A.solve2var1sol (form1,form2)



{--------------------}
{- Ordered Geometry -}
{--------------------}

instance OrderedGeometry CartesianPlane where
  isBetween (P x) (P a, P b) = and
    [ x /= a && x /= b
    , A.isSolutionOf2 x (A.seEndpoints a b)
    ]


  interpolate (P a) (P b) = if a == b
    then Nothing
    else
      let
        Just eq = A.lfPointPoint a b
        x = A.interpolate (-3/2) a b
        y = A.interpolate (1/2)  a b
        z = A.interpolate (3/2)  a b
      in Just (P x, P y, P z)



  onSameSideOf (P u) (P v) (P a, P b) =
    let
      du = A.orientation a b u
      dv = A.orientation a b v
    in
      if du == A.Zero || dv == A.Zero
        then False
        else du == dv



{-----------------------}
{- Congruence Geometry -}
{-----------------------}

instance CongruenceGeometry CartesianPlane where
  segmentCongruence (P a, P b) (P x, P y) =
    let
      h = A.dotSq $ a A.-. b
      k = A.dotSq $ x A.-. y
    in h == k


  angleCongruence (P a, P o, P b) (P u, P p, P v) =
    let
      d = (A.dotSq $ a A.-. o) * (A.dotSq $ b A.-. o)
      e = (A.dotSq $ u A.-. p) * (A.dotSq $ v A.-. p)

      h = ((a A.-. o) `A.dot` (b A.-. o))^2 / d
      k = ((u A.-. p) `A.dot` (v A.-. p))^2 / e
    in
      if d==0 || e==0
        then d==e
        else h==k



{------------------}
{- Plane Geometry -}
{------------------}

instance PlaneGeometry CartesianPlane where
  cutCircleRay (P o, P a) (P b) =
    fmap P $ A.projectPtCE b (A.ceCenterPoint o a)


  cutCircles (a,o,b) (x,p,y) =
    if not $ isBetween x (a,b) && isBetween b (x,y)
      then Nothing
      else do
        let
          c1 = A.ceDiameter (unP a) (unP b)
          c2 = A.ceDiameter (unP x) (unP y)
          A.Only2of2 u v = A.solve2var2sol (c1,c2)
        Just (P u, P v)



{--------}
{- View -}
{--------}

instance ToCartesian CartesianPlane where
  playfield = Playfield []

  toCartesian command =
    let
      smear a = A.approximateWithin (1/1000) a
      fudge (x,y) = (smear x, smear y)
    in
      case command of
        DrawPoint s (P x) ->
          [CartesianPoint s (fudge x)]
        DrawLine s (P x) (P y) ->
          [CartesianLine s (fudge x) (fudge y)]
        DrawSegment s (P x) (P y) ->
          [CartesianSegment s (fudge x) (fudge y)]
        DrawRay s (P x) (P y) ->
          [CartesianRay s (fudge x) (fudge y)]
        DrawCircle s (P x) (P y) ->
          [CartesianCircle s (fudge x) (smear $ A.distance x y)]
