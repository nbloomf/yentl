#!/usr/bin/runhaskell -i/home/nathan/code/yentl/src/

import Yentl

fig :: (Coords t, PlaneGeometry t) => Fig t ()
fig = do
  a <- coords (-1,1) >>= pen blue
  b <- coords (-2,2) >>= pen blue
  seg <- segment a b >>= pen blue

  o <- coords (2,3) >>= pen green
  p <- coords (3,5) >>= pen green
  ray <- ray o p >>= pen green

  c1 <- circle a b >>= pen ultrathin

  if a == o
    then do
      projectPointOnCircle p c1
      return ()
    else do
      (u,_) <- equilateralPoints a o
      pen plain u
      v <- pointBeyond u a
      w <- projectPointOnCircle v c1
      c2 <- circle u w >>= pen ultrathin
      s <- projectPointOnCircle o c2
      c3 <- circle o s >>= pen ultrathin
      projectPointOnCircle p c3 >>= pen red
      return ()


vOpts = fixed ((-5,-1/2),(7,10)) defaultView


main :: IO ()
main = do
  writeImg (fig :: Fig CartesianPlane ()) EPS vOpts "test002/" "002cartesian.eps" $
    "Copying an angle in the cartesian plane.\n"
  writeImg (fig :: Fig PoincarePlane ()) EPS vOpts "test002/" "002poincare.eps" $
    "Copying an angle in the cartesian plane.\n"
