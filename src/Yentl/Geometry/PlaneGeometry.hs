{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Yentl.Geometry.PlaneGeometry (
  PlaneGeometry,
  cutCircleRay, cutCircles,

  projectPointOnCircle
) where


import Yentl.Geo
import Yentl.Verify
import Yentl.Geometry.IncidenceGeometry
import Yentl.Geometry.OrderedGeometry
import Yentl.Geometry.CongruenceGeometry



{-----------------}
{- PlaneGeometry -}
{-----------------}

class (CongruenceGeometry t) => PlaneGeometry t where
  -- intersect a circle with a central ray
  cutCircleRay :: (t,t) -> t -> Maybe t

  -- intersect circles from interleaved diameters
  cutCircles :: (t,t,t) -> (t,t,t) -> Maybe (t,t)



{- Instances -}

instance (PlaneGeometry t) => IsInteriorTo t (Circle t) where
  isInteriorTo x c =
    let o = centerOf c in
    let a = pointOn c in
    if x == o
      then True
      else case cutCircleRay (o,a) x of
        Nothing -> False
        Just c  -> isBetween x (o,c)



{-------------}
{- Utilities -}
{-------------}

projectPointOnCircle :: (PlaneGeometry t) => t -> Circle t -> Fig t t
projectPointOnCircle x c = do
  let o = centerOf c
  let a = pointOn c
  if x == o
    then report $ PointsEqual x o
    else do
      let Just p = cutCircleRay (o,a) x
      return p
