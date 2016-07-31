module Yentl.View.Cartesian where

import Yentl.Algebra
import Yentl.View.Command
import Yentl.View.Options

data Cartesian
  = CartesianPoint
      Style (Rational, Rational)
  | CartesianSegment
      Style (Rational, Rational) (Rational, Rational)
  | CartesianLine
      Style (Rational, Rational) (Rational, Rational)
  | CartesianRay
      Style (Rational, Rational) (Rational, Rational)
  | CartesianCircle
      Style (Rational, Rational) Rational
  | CartesianArc
      Style (Rational, Rational) Rational Angle Angle -- from/to, counterclockwise
  deriving Show

type Angle = Rational


data Playfield t = Playfield [Cartesian]

playfieldType :: [Command t] -> Playfield t -> [Cartesian]
playfieldType _ (Playfield xs) = xs

getPlayfield :: (ToCartesian t) => [Command t] -> [Cartesian]
getPlayfield xs = playfieldType xs playfield

class ToCartesian t where
  playfield :: Playfield t
  toCartesian :: Command t -> [Cartesian]

toViewWindow' :: (ToCartesian t) => ViewOpts -> [Command t] -> [Cartesian]
toViewWindow' opts xs = toViewWindow opts $ (concatMap toCartesian xs) ++ (getPlayfield xs)





toViewWindow :: ViewOpts -> [Cartesian] -> [Cartesian]
toViewWindow (ViewOpts bound (minV@(minVx, minVy), maxV@(maxVx, maxVy)) _) xs = map xform xs
  where
    xform (CartesianPoint s p) = CartesianPoint s (affine p)
    xform (CartesianSegment s p q) = CartesianSegment s (affine p) (affine q)
    xform (CartesianLine s p q) = CartesianLine s (affine p) (affine q)
    xform (CartesianRay s p q) = CartesianRay s (affine p) (affine q)
    xform (CartesianCircle s o r) = CartesianCircle s (affine o) (scale * r)
    xform (CartesianArc s o r a b) = CartesianArc s (affine o) (scale * r) a b

    scale = min (heightV / heightB) (widthV / widthB)

    (minB@(minBx, minBy), maxB@(maxBx, maxBy)) = case bound of
      Fixed (p,q)    -> (p,q)
      BestFit margin -> computeBestFit margin xs

    heightV = maxVy - minVy
    widthV  = maxVx - minVx

    heightB =
      let x = maxBy - minBy in
      if x == 0
        then error "heightB is zero"
        else x

    widthB =
      let x = maxBx - minBx in
      if x == 0
        then error "widthB is zero"
        else x


    centerB = midpoint minB maxB
    centerV = midpoint minV maxV

    affine p = (scale @. (p -. centerB)) +. centerV

interestingPoints :: [Cartesian] -> [(Rational, Rational)]
interestingPoints = concatMap foo
  where
    foo :: Cartesian -> [(Rational, Rational)]
    foo (CartesianPoint _ p)     = [p]
    foo (CartesianSegment _ p q) = [p,q]
    foo (CartesianLine _ p q)    = [p,q]
    foo (CartesianRay _ p q)     = [p,q]
    foo (CartesianCircle _ o r)  = [o -. m, o +. m]
      where m = (r,r)
    foo (CartesianArc _ o r _ _)  = [o -. m, o +. m]
      where m = (r,r)

computeBestFit :: Rational -> [Cartesian] -> ((Rational, Rational), (Rational, Rational))
computeBestFit margin xs = (lowerLeft box, upperRight box)
  where box = boundingBox margin $ interestingPoints xs
