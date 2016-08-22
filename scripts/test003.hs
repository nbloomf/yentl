#!/usr/bin/runhaskell -i/home/nathan/code/yentl/src/

import Yentl

fig :: Rational -> Fig (PoincareDisc Rational) ()
fig t = do
  let c1 = path (CirclePath (0,0) (2/3) 0 (-1)) t
  p1 <- coords $ path (CirclePath c1 (2/9) (0/3) 2) t
  p2 <- coords $ path (CirclePath c1 (2/9) (1/3) 2) t
  (p3,p4) <- equilateralPoints p1 p2
  let
    eqButNot x (a,b) = do
      (u,v) <- equilateralPoints a b
      if u == x
        then return v
        else return u
  p5 <- eqButNot p1 (p2,p3)
  p6 <- eqButNot p2 (p1,p3)
  p7 <- eqButNot p1 (p2,p4)
  p8 <- eqButNot p2 (p1,p4)
  let
    ps = [p1,p2,p3,p4,p5,p6,p7,p8]
    segs =
      [ (p1,p2), (p1,p3), (p1,p4), (p2,p3)
      , (p2,p4), (p2,p5), (p3,p5), (p1,p6)
      , (p3,p6), (p2,p7), (p4,p7), (p1,p8)
      , (p4,p8)
      ]
  let eff (a,b) = segment a b >>= pen plain
  sequence $ map eff segs
  sequence $ map (pen plain) ps
  return ()


vOpts = discView
aOpts = frames 100 defaultAnimate


main :: IO ()
main = do
  writeAni fig vOpts aOpts "test003/" "003" $
    "Equilateral triangles in the poincare disc.\n"
