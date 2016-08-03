module Yentl.View.Options (
  ViewOpts(..), BoundingBoxOpt(..),
  defaultView, discView, fixed, bestFit,

  AnimateOpts(..),
  defaultAnimate, startAt, endAt, frames
) where


{----------------}
{- View Options -}
{----------------}

data ViewOpts = ViewOpts
  { optBoundingBox :: BoundingBoxOpt
  , optViewWindow  :: ((Rational, Rational), (Rational, Rational))
  , optPageSize    :: (Int, Int)
  } deriving Show

data BoundingBoxOpt
  = Fixed ((Rational, Rational), (Rational, Rational))
  | BestFit Rational -- margin
  deriving Show

defaultView :: ViewOpts
defaultView = ViewOpts
  { optBoundingBox = Fixed ((-20,-20),(20,20))
  , optViewWindow  = ((0,0),(200,200))
  , optPageSize    = (200, 200)
  }

discView :: ViewOpts
discView = ViewOpts
  { optBoundingBox = Fixed ((-11/10,-11/10),(11/10,11/10))  
  , optViewWindow  = ((0,0),(200,200))
  , optPageSize    = (200, 200)
  }

fixed :: ((Rational, Rational), (Rational, Rational)) -> ViewOpts -> ViewOpts
fixed bounds opts = opts { optBoundingBox = Fixed bounds }

bestFit :: Rational -> ViewOpts -> ViewOpts
bestFit margin opts = opts { optBoundingBox = BestFit margin }



{-------------------}
{- Animate Options -}
{-------------------}

data AnimateOpts = AnimateOpts
  { optStartT    :: Rational
  , optEndT      :: Rational
  , optNumFrames :: Int
  }

defaultAnimate = AnimateOpts
  { optStartT    = 0
  , optEndT      = 1
  , optNumFrames = 30
  }

startAt :: Rational -> AnimateOpts -> AnimateOpts
startAt x opts = opts { optStartT = x }

endAt :: Rational -> AnimateOpts -> AnimateOpts
endAt x opts = opts { optEndT = x }

frames :: Int -> AnimateOpts -> AnimateOpts
frames k opts = opts { optNumFrames = k }
