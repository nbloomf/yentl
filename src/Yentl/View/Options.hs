module Yentl.View.Options (
  ViewOpts(..),
  defaultViewOptsFixed,
  defaultViewOptsBestFit,
  BoundingBoxOpt(..),

  AnimateOpts(..),
  defaultAnimateOpts
) where


data ViewOpts = ViewOpts
  { optBoundingBox :: BoundingBoxOpt
  , optViewWindow  :: ((Rational, Rational), (Rational, Rational))
  , optPageSize    :: (Int, Int)
  }
 
defaultViewOptsFixed :: ViewOpts
defaultViewOptsFixed = ViewOpts
  { optBoundingBox = Fixed ((-20,-20),(20,20))
  , optViewWindow  = ((0,0),(200,200))
  , optPageSize    = (200, 200)
  }

defaultViewOptsBestFit :: ViewOpts
defaultViewOptsBestFit = ViewOpts
  { optBoundingBox = BestFit 10  
  , optViewWindow  = ((0,0),(200,200))
  , optPageSize    = (200, 200)
  }

data BoundingBoxOpt
  = Fixed ((Rational, Rational), (Rational, Rational))
  | BestFit Rational -- margin


data AnimateOpts = AnimateOpts
  { optStartT    :: Rational
  , optEndT      :: Rational
  , optNumFrames :: Int
  }

defaultAnimateOpts = AnimateOpts
  { optStartT    = 0
  , optEndT      = 1
  , optNumFrames = 30
  }
