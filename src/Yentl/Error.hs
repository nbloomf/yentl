module Yentl.Error where

data GeoError t
  = PointsEqual t t
  | LinesNotIncident (t,t) (t,t)
  | PointNotBetween t (t,t)
  | BadCoordinates (Rational, Rational)
  | AssertionFailure String
  | CirclesNotIncident (t,t) (t,t)
  deriving Show
