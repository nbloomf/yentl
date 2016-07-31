{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Yentl.Geometry.IncidenceGeometry (
  IncidenceGeometry,
  areCollinear, intersectLines,

  -- classes
  LiesOn, liesOn,
  HasPoint, pointOn,
  CommonPoint, commonPoint,

  -- helper types
  Line(), lineMaybe, line, twoPointsOnLine,
  ContainedInLine, lineContaining,

  -- utilities
  areAllCollinear, areIncident, areParallel
) where

import Data.Maybe (isJust)

import Yentl.Geo
import Yentl.Verify


{---------------------}
{- IncidenceGeometry -}
{---------------------}

class (Eq t) => IncidenceGeometry t where
  -- detects whether three points are collinear
  areCollinear :: t -> t -> t -> Bool

  -- returns the intersection of two lines.
  intersectLines :: (t,t) -> (t,t) -> Maybe t


instance (IncidenceGeometry t) => Draw t t where
  draw x = [DrawPoint plain x]



{-----------}
{- Classes -}
{-----------}

-- when it makes sense for a point to "lie on" a thing
class (IncidenceGeometry t) => LiesOn t a | a -> t where
  liesOn :: t -> a -> Bool

class (IncidenceGeometry t) => HasPoint t a | a -> t where
  pointOn :: a -> t

-- for loci which may have a unique point in common
class (LiesOn t a, LiesOn t b, IncidenceGeometry t)
  => CommonPoint t a b | a -> t, b -> t where
  -- computes the common point of two loci
  -- fails catastrophically if it does not exist
  commonPoint :: a -> b -> Fig t t



{--------}
{- Line -}
{--------}

data Line t = Line t t

{- Destructor -}

twoPointsOnLine :: Line t -> (t,t)
twoPointsOnLine (Line a b) = (a,b)

{- Constructor -}

lineMaybe :: (Eq t) => t -> t -> Maybe (Line t)
lineMaybe x y = if x == y
  then Nothing
  else Just $ Line x y

line :: (IncidenceGeometry t) => t -> t -> Fig t (Line t)
line x y = if x == y
  then report $ PointsEqual x y
  else return $ Line x y

{- Instances -}

instance (Show t) => Show (Line t) where
  show (Line x y) = "line(" ++ show x ++ "," ++ show y ++ ")"

instance (IncidenceGeometry t) => LiesOn t (Line t) where
  liesOn x (Line a b) = if a==b
    then error "LiesOn instance for Line (liesOn)"
    else (x == a) || (x == b) || (areCollinear x a b)

instance (IncidenceGeometry t) => Eq (Line t) where
  (Line a b) == (Line x y) = if a==b || x==y
    then error "Eq instance for Line (==)"
    else (areCollinear a x y) && (areCollinear b x y)

instance (IncidenceGeometry t) => HasPoint t (Line t) where
  pointOn (Line x _) = x

instance (IncidenceGeometry t) => CommonPoint t (Line t) (Line t) where
  commonPoint (Line a1 b1) (Line a2 b2) = do
    case intersectLines (a1,b1) (a2,b2) of
      Nothing -> report $ LinesNotIncident (a1,b1) (a2,b2)
      Just x  -> return x

instance (IncidenceGeometry t) => Draw t (Line t) where
  draw (Line x y) = [DrawLine plain x y]

{- Classes -}

class (IncidenceGeometry t) => ContainedInLine t a | a -> t where
  lineContaining :: a -> Fig t (Line t)

instance (IncidenceGeometry t) => ContainedInLine t (Line t) where
  lineContaining = return



{-------------}
{- Utilities -}
{-------------}

-- true iff there is a line containing pts in list
areAllCollinear :: (IncidenceGeometry t) => [t] -> Bool
areAllCollinear (x:y:z:rest)
  = (areCollinear x y z) && (areAllCollinear (y:z:rest))
areAllCollinear _ = True

-- true iff lines have unique point in common
areIncident :: (IncidenceGeometry t) => Line t -> Line t -> Bool
areIncident l1 l2 =
  let ab = twoPointsOnLine l1 in
  let uv = twoPointsOnLine l2 in
  isJust $ intersectLines ab uv

-- true iff lines have no points in common
areParallel :: (IncidenceGeometry t) => Line t -> Line t -> Bool
areParallel l1 l2 = if l1 == l2
  then False
  else not $ areIncident l1 l2
