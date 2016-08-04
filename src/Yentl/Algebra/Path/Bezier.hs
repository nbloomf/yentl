module Yentl.Algebra.Path.Bezier where


import Yentl.Algebra.Classes
import Yentl.Algebra.Number.Integer
import Yentl.Algebra.Number.Rational


data Path a
  = LinearPath (a,a) (a,a)               -- start end
  | CirclePath (a,a) a Rational Rational -- center radius start-angle speed
  | BezierPath [(a,a)]                   -- control points


path :: (Ord a, Num a, Fractional a) => Path a -> a -> (a,a)
path p t = case p of
  LinearPath a b -> a +. (t @. (b -. a))

  CirclePath (ox,oy) r a s ->
    ( ox + r*(cosRatD $ 360*(fromRational s)*(t + (fromRational a)))
    , oy + r*(sinRatD $ 360*(fromRational s)*(t + (fromRational a)))
    )

  BezierPath ps -> modSum $ zipWith (@.) coef ps
    where
      n = (fromIntegral $ length ps) - 1
      coef = [(fromInteger $ binomial n k) * (1-t)^(n-k) * t^k | k <- [0..n]]
