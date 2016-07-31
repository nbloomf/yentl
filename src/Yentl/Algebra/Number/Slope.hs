{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Yentl.Algebra.Number.Slope (
  Slope(), deltaY, deltaX, slope, perp,

  HasSlope, slopeOf, sameSlope, slopeBetween
) where



{---------}
{- Slope -}
{---------}

data Slope a = Slope
  { dy :: a
  , dx :: a
  }

deltaY = dy
deltaX = dx


-- Safe Constructor
slope :: (Eq a, Num a) => a -> a -> Maybe (Slope a)
slope x y = if (x,y) == (0,0)
  then Nothing
  else Just $ Slope { dx = x, dy = y }


perp :: (Num a) => Slope a -> Slope a
perp s = Slope { dy = -(dx s), dx = dy s }



{-------------}
{- Instances -}
{-------------}

instance (Show a) => Show (Slope a) where
  show s = show (dy s) ++ "::" ++ show (dx s)

instance (Eq a, Num a) => Eq (Slope a) where
  m1 == m2 = (dy m1)*(dx m2) == (dy m2)*(dx m1)



{------------}
{- HasSlope -}
{------------}

class HasSlope t a | t -> a where
  slopeOf :: t -> Maybe (Slope a)


slopeBetween :: (Eq a, Num a) => (a,a) -> (a,a) -> Maybe (Slope a)
slopeBetween (x1,y1) (x2,y2) = slope (x2 - x1) (y2 -  y1)

sameSlope :: (Eq a, Num a)
  => ((a,a),(a,a),(a,a)) -> Bool
sameSlope (p,q,r) = case (slopeBetween p q, slopeBetween p r) of
  (Just m1, Just m2) -> m1 == m2
  _ -> True

