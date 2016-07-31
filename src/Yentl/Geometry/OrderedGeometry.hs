{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Yentl.Geometry.OrderedGeometry (
  OrderedGeometry,
  isBetween, interpolate, onSameSideOf,

  -- classes
  IsInteriorTo, isInteriorTo, isExteriorTo,
  HasVertex, vertexOf,

  -- helper types
  Segment(), segment, segmentMaybe, endpointsOf,
  Ray(), ray, rayMaybe,
  Angle(), angle, initialSideOf, terminalSideOf,
  Triangle(), triangle,
  triangleVertices, triangleSides, triangleAngles,

  -- utilities
  areInOrder,
  pointBefore, pointBetween, pointBeyond
) where


import Yentl.Geo
import Yentl.Verify
import Yentl.Geometry.IncidenceGeometry



{-------------------}
{- OrderedGeometry -}
{-------------------}

class (IncidenceGeometry t) => OrderedGeometry t where
  -- detect whether a point is between two others
  isBetween :: t -> (t,t) -> Bool

  -- return points in the regions separated by two points
  interpolate :: t -> t -> Maybe (t,t,t)

  -- detect whether two points are on the same side of a line
  onSameSideOf :: t -> t -> (t,t) -> Bool



class (LiesOn t a) => IsInteriorTo t a | a -> t where
  isInteriorTo :: t -> a -> Bool

  isExteriorTo :: t -> a -> Bool
  isExteriorTo t a = not (liesOn t a || isInteriorTo t a)

class HasVertex t a | a -> t where
  vertexOf :: a -> t



{-----------}
{- Segment -}
{-----------}

data Segment t = Segment t t

{- Destructor -}

endpointsOf :: Segment t -> (t,t)
endpointsOf (Segment a b) = (a,b)

{- Constructor -}

segmentMaybe :: (Eq t) => t -> t -> Maybe (Segment t)
segmentMaybe a b = if a == b
  then Nothing
  else Just $ Segment a b

segment :: (OrderedGeometry t) => t -> t -> Fig t (Segment t)
segment a b = if a == b
  then report $ PointsEqual a b
  else return $ Segment a b

{- Instances -}

instance (Show t) => Show (Segment t) where
  show (Segment a b) = "seg(" ++ show a ++ "," ++ show b ++ ")"

instance (OrderedGeometry t) => LiesOn t (Segment t) where
  liesOn x (Segment a b) = or
    [ x == a
    , x == b
    , isBetween x (a,b)
    ]

instance (OrderedGeometry t) => Eq (Segment t) where
  (Segment a b) == (Segment x y) = or
    [ a == x && b == y
    , a == y && b == x
    ]

instance (OrderedGeometry t) => ContainedInLine t (Segment t) where
  lineContaining (Segment a b) = line a b

instance Draw t (Segment t) where
  draw (Segment x y) = [DrawSegment plain x y]



{-------}
{- Ray -}
{-------}

data Ray t = Ray t t

{- Destructors -}

instance HasVertex t (Ray t) where
  vertexOf (Ray o _) = o

instance (OrderedGeometry t) => HasPoint t (Ray t) where
  pointOn (Ray _ x) = x

{- Constructor -}

rayMaybe :: (Eq t) => t -> t -> Maybe (Ray t)
rayMaybe a b = if a == b
  then Nothing
  else Just $ Ray a b

ray :: (OrderedGeometry t) => t -> t -> Fig t (Ray t)
ray o a = if o == a
  then report $ PointsEqual o a
  else return $ Ray o a

{- Instances -}

instance (Show t) => Show (Ray t) where
  show (Ray a b) = "ray(" ++ show a ++ "," ++ show b ++ ")"

instance (OrderedGeometry t) => LiesOn t (Ray t) where
  liesOn x (Ray o a) = or
    [ x == o
    , x == a
    , isBetween x (o,a)
    , isBetween a (o,x)
    ]

instance (OrderedGeometry t) => Eq (Ray t) where
  (Ray o a) == (Ray p b) = and
    [ o == p
    , a /= p
    , a `liesOn` (Ray p b)
    ]

instance (OrderedGeometry t) => ContainedInLine t (Ray t) where
  lineContaining (Ray o a) = line o a

instance Draw t (Ray t) where
  draw (Ray o a) = [DrawRay plain o a]



{---------}
{- Angle -}
{---------}

data Angle t = Angle t t t

{- Constructor -}

angle :: (OrderedGeometry t) => t -> t -> t -> Fig t (Angle t)
angle a o b = if a == o
  then report $ PointsEqual a o
  else if b == o
    then report $ PointsEqual b o
    else return $ Angle a o b

{- Destructors -}

instance (OrderedGeometry t) => HasVertex t (Angle t) where
  vertexOf (Angle _ o _) = o

initialSideOf :: (OrderedGeometry t) => Angle t -> Ray t
initialSideOf (Angle a o _) = Ray o a

terminalSideOf :: (OrderedGeometry t) => Angle t -> Ray t
terminalSideOf (Angle _ o b) = Ray o b

{- Instances -}

instance (OrderedGeometry t) => LiesOn t (Angle t) where
  liesOn x (Angle a o b) = or
    [ x == o
    , x `liesOn` (Ray o a)
    , x `liesOn` (Ray o b)
    ]

instance (OrderedGeometry t) => Eq (Angle t) where
  (Angle a o b) == (Angle x p y) = or
    [ o == p && (Ray o a)==(Ray p x) && (Ray o b)==(Ray p y)
    , o == p && (Ray o a)==(Ray p y) && (Ray o b)==(Ray p x)
    ]

instance Draw t (Angle t) where
  draw (Angle a o b) = [DrawRay plain o a, DrawRay plain o b]

instance (OrderedGeometry t) => IsInteriorTo t (Angle t) where
  isInteriorTo x (Angle a o b) =
    (onSameSideOf a x (o,b)) && (onSameSideOf b x (o,a))



{------------}
{- Triangle -}
{------------}

data Triangle t = Triangle t t t

{- Constructor -}

triangle :: (OrderedGeometry t)
  => t -> t -> t -> Fig t (Triangle t)
triangle a b c = if a == b
  then report $ PointsEqual a b
  else if b == c
    then report $ PointsEqual b c
    else if c == a
      then report $ PointsEqual c a
      else return $ Triangle a b c

{- Destructors -}

triangleVertices :: (OrderedGeometry t)
  => Triangle t -> (t,t,t)
triangleVertices (Triangle a b c) = (a,b,c)

triangleSides :: (OrderedGeometry t)
  => Triangle t -> (Segment t, Segment t, Segment t)
triangleSides (Triangle a b c) = 
  (Segment a b, Segment b c, Segment c a)

triangleAngles :: (OrderedGeometry t)
  => Triangle t -> (Angle t, Angle t, Angle t)
triangleAngles (Triangle a b c) =
  (Angle b a c, Angle c b a, Angle a c b)

{- Instances -}

instance (OrderedGeometry t) => Eq (Triangle t) where
  t1 == t2 = vtx1 `elem` [(a,b,c),(a,c,b),(b,a,c),(b,c,a),(c,a,b),(c,b,a)]
    where
      vtx1 = triangleVertices t1
      (a,b,c) = triangleVertices t2

instance (OrderedGeometry t) => LiesOn t (Triangle t) where
  liesOn x tri =
    let (s1,s2,s3) = triangleSides tri in
    (liesOn x s1) || (liesOn x s2) || (liesOn x s3)

instance (OrderedGeometry t) => IsInteriorTo t (Triangle t) where
  isInteriorTo x tri =
    let (a1,a2,a3) = triangleAngles tri in
    (isInteriorTo x a1) && (isInteriorTo x a2) && (isInteriorTo x a3)

instance Draw t (Triangle t) where
  draw (Triangle a b c) =
    [ DrawSegment plain a b
    , DrawSegment plain b c
    , DrawSegment plain c a
    ]



{---------------------}
{- Derived Utilities -}
{---------------------}

areInOrder :: (OrderedGeometry t) => [t] -> Bool
areInOrder (x:y:z:rest) = (isBetween y (x,z)) && areInOrder (y:z:rest)
areInOrder _ = True

pointBefore :: (OrderedGeometry t) => t -> t -> Fig t t
pointBefore a b = case interpolate a b of
  Just (x,_,_) -> return x
  Nothing -> report $ PointsEqual a b

pointBetween :: (OrderedGeometry t) => t -> t -> Fig t t
pointBetween a b = case interpolate a b of
  Just (_,x,_) -> return x
  Nothing -> report $ PointsEqual a b

pointBeyond :: (OrderedGeometry t) => t -> t -> Fig t t
pointBeyond a b = case interpolate a b of
  Just (_,_,x) -> return x
  Nothing -> report $ PointsEqual a b

verticalAngle :: (OrderedGeometry t) => Angle t -> Fig t (Angle t)
verticalAngle ang = do
  let a = pointOn $ initialSideOf ang
  let o = vertexOf ang
  let b = pointOn $ terminalSideOf ang
  x <- pointBefore o b
  y <- pointBefore o a
  angle x o y
