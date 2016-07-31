module Yentl.Theorems.PlaneGeometry where

import Yentl.Geo
import Yentl.Geometry.IncidenceGeometry
import Yentl.Theorems.IncidenceGeometry
import Yentl.Geometry.OrderedGeometry
import Yentl.Theorems.OrderedGeometry
import Yentl.Geometry.CongruenceGeometry
import Yentl.Theorems.CongruenceGeometry
import Yentl.Geometry.PlaneGeometry


{----------------------}
{- construct antipode -}
{----------------------}

-- copy a point onto the opposite ray of another
-- let a and o be points. there is a unique point b
-- such that [aob] and seg(ao) == seg(ob); this
-- b is called the antipode of a.
antipodeThrough
  :: (PlaneGeometry t) => t -> t -> Fig t t
antipodeThrough a o = do
  if a == o
    then return o
    else do
      x <- pointBefore o a
      let Just b = cutCircleRay (o,a) x
      return b



-- if b is the antipode of a through o then seg(oa) == seg(ob)
thmAntipodeCongruence
  :: (PlaneGeometry t) => t -> t -> Fig t ()
thmAntipodeCongruence a o = do
  let True = a /= o
  b <- antipodeThrough a o
  s1 <- segment o a
  s2 <- segment o b
  let True = s1 `isCongruentTo` s2
  return ()



-- if b is the antipode of a through o then [aob]
thmAntipodeBetween :: (PlaneGeometry t)
  => t -> t -> Fig t ()
thmAntipodeBetween a o = do
  let True = a /= o
  b <- antipodeThrough a o
  let True = o `isBetween` (a,b)
  return ()



{----------------------}
{- equilateral points -}
{----------------------}

-- construct the two points which form equilateral triangles with two others
equilateralPoints :: (PlaneGeometry t)
  => t -> t -> Fig t (t,t)
equilateralPoints a b = do
  x <- antipodeThrough b a
  y <- antipodeThrough a b
  let Just (p,q) = cutCircles (x,a,b) (a,b,y)
  return (p,q)



-- equilateral points form 5 congruent segments
thmEquilateralPointsCongruence :: (PlaneGeometry t)
  => t -> t -> Fig t ()
thmEquilateralPointsCongruence a b = do
  let True = a /= b
  (x,y) <- equilateralPoints a b
  s1 <- segment a b
  s2 <- segment a x
  s3 <- segment a y
  s4 <- segment b x
  s5 <- segment b y
  let True = areAllCongruent [s1,s2,s3,s4,s5]
  return ()



-- equilateral points of a and b are on opposite sides of line(a,b)
thmEquilateralPointsOppositeSides :: (PlaneGeometry t)
  => t -> t -> Fig t ()
thmEquilateralPointsOppositeSides a b = do
  let True = a /= b
  (x,y) <- equilateralPoints a b
  let True = not $ onSameSideOf x y (a,b)
  return ()



{-------------------------}
{- copy segment onto ray -}
{-------------------------}

-- copy a segment onto a ray
copySegmentToRay :: (PlaneGeometry t)
  => Segment t -> Ray t -> Fig t t
copySegmentToRay seg ray = do
  let (a,b) = endpointsOf seg
  c1 <- circle a b
  let (o,p) = (vertexOf ray, pointOn ray)
  if a == o
    then projectPointOnCircle p c1
    else do
      (u,_) <- equilateralPoints a o
      v <- pointBeyond u a
      w <- projectPointOnCircle v c1
      c2 <- circle u w
      s <- projectPointOnCircle o c2
      c3 <- circle o s
      projectPointOnCircle p c3



-- copying segment yields congruent segment
thmCopySegmentToRayCongruence :: (PlaneGeometry t)
  => Segment t -> Ray t -> Fig t ()
thmCopySegmentToRayCongruence seg ray = do
  let o = vertexOf ray
  t <- copySegmentToRay seg ray
  s <- segment o t
  let True = s `isCongruentTo` seg
  return ()






{-
copyAngleOntoRay :: (PlaneGeometry t)
  => Angle t -> Ray t -> Fig t (t,t)
copyAngleOntoRay ang xy = do
  let (a,o,b) = (pointOn $ initialSideOf ang, vertexOf ang, pointOn $ terminalSideOf ang)
  let (x,y) = (vertexOf xy, pointOn xy)
  oa <- segment o a
  u  <- copySegmentOntoRay oa xy
  ab <- segment a b
  ux <- ray u x
  v  <- copySegmentOntoRay ab ux
  ob <- segment o b
  w  <- copySegmentOntoRay ob xy
  c  <- circle u v
  d  <- circle x w
  cutTwoCircles c d

-}
