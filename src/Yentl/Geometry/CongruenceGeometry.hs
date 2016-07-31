{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Yentl.Geometry.CongruenceGeometry (
  CongruenceGeometry,
  segmentCongruence, angleCongruence,

  -- classes
  Congruence, isCongruentTo, areAllCongruent,
  HasCenter, centerOf,

  -- helper types
  Circle(), circle, circleMaybe
) where

import Yentl.Geo
import Yentl.Geometry.IncidenceGeometry
import Yentl.Geometry.OrderedGeometry



{----------------------}
{- CongruenceGeometry -}
{----------------------}

class (OrderedGeometry t) => CongruenceGeometry t where
  segmentCongruence :: (t,t) -> (t,t) -> Bool

  angleCongruence :: (t,t,t) -> (t,t,t) -> Bool



{-----------}
{- Classes -}
{-----------}

class (CongruenceGeometry t) => Congruence t a | a -> t where
  isCongruentTo :: a -> a -> Bool

  areAllCongruent :: [a] -> Bool
  areAllCongruent (x:y:rest) =
    (isCongruentTo x y) && areAllCongruent (y:rest)
  areAllCongruent _ = True

class (CongruenceGeometry t) => HasCenter t a | a -> t where
  centerOf :: a -> t



{----------}
{- Circle -}
{----------}

data Circle t = Circle t t

{- Constructor -}

circleMaybe :: (Eq t) => t -> t -> Maybe (Circle t)
circleMaybe o a = if o == a
  then Nothing
  else Just $ Circle o a

circle :: (CongruenceGeometry t) => t -> t -> Fig t (Circle t)
circle o a = if a == o
  then report $ PointsEqual a o
  else return $ Circle o a

{- Destructors -}

instance (CongruenceGeometry t) => HasCenter t (Circle t) where
  centerOf (Circle o _) = o

instance (CongruenceGeometry t) => HasPoint t (Circle t) where
  pointOn (Circle _ a) = a

{- Instances -}

instance (Show t) => Show (Circle t) where
  show (Circle o a) = "circ(" ++ show o ++ "," ++ show a ++ ")"

instance (CongruenceGeometry t) => Draw t (Circle t) where
  draw (Circle o a) = [DrawCircle plain o a]

instance (CongruenceGeometry t) => LiesOn t (Circle t) where
  liesOn x (Circle o a) = segmentCongruence (o,x) (o,a)



{--------------}
{- Congruence -}
{--------------}

instance (CongruenceGeometry t) => Congruence t (Segment t) where
  isCongruentTo s1 s2 =
    segmentCongruence (endpointsOf s1) (endpointsOf s2)

instance (CongruenceGeometry t) => Congruence t (Angle t) where
  isCongruentTo a1 a2 =
    let x1 = pointOn $ initialSideOf a1 in
    let o1 = vertexOf a1 in
    let y1 = pointOn $ terminalSideOf a1 in
    let x2 = pointOn $ initialSideOf a2 in
    let o2 = vertexOf a2 in
    let y2 = pointOn $ terminalSideOf a2 in
    angleCongruence (x1,o1,y1) (x2,o2,y2)

instance (CongruenceGeometry t) => Congruence t (Circle t) where
  isCongruentTo c1 c2 =
    let (o1,x1) = (centerOf c1, pointOn c1) in
    let (o2,x2) = (centerOf c2, pointOn c2) in
    segmentCongruence (o1,x1) (o2,x2)
