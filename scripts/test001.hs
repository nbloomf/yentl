#!/usr/bin/runhaskell -i/home/nathan/code/yentl/src/

import Yentl


fig :: Fig PoincarePlane ()
fig = do
  p <- coords (1,1) >>= pen plain
  q <- coords (-2,2) >>= pen plain
  r <- coords (0,3) >>= pen plain
  segment p q >>= pen plain
  segment q r >>= pen plain
  segment r p >>= pen plain
  return ()

vOpts = fixed ((-3,-1),(3,4)) defaultView


main :: IO ()
main = writeImg fig EPS vOpts "test001/" "001.eps" $
  "Drawing of a triangle in the poincare plane.\n"
