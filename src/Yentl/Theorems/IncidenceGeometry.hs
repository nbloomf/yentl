module Yentl.Theorems.IncidenceGeometry where

import Yentl.Geo
import Yentl.Verify
import Yentl.Geometry.IncidenceGeometry

thmLineTrichotomy :: (IncidenceGeometry t)
  => Line t -> Line t -> Bool
thmLineTrichotomy l1 l2 = exactlyOne
  [ l1 == l2
  , areIncident l1 l2
  , areParallel l1 l2
  ]
