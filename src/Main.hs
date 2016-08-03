module Main where

import Yentl
import Demo

main :: IO ()
main = demo


{-


fig2 :: Fig (PoincareDisc ConReal) ()
fig2 = do
  p <- coords 0 0
  q <- coords (1/2) (1/3)
  r <- coords (-3/4) (1/12)
  segment p q >>= pen plain
  segment q r >>= pen plain
  segment r p >>= pen plain
  return ()


fig1 :: Fig PoincarePlane ()
fig1 = do
  p <- coords 0 0 >>= pen (plain <& blue)
  q <- coords 100 100 >>= pen plain
  r <- coords 50 100 >>= pen plain
  s <- segment p q >>= pen plain
  t <- segment p r >>= pen blue
  c <- circle p q >>= pen plain
  d <- circle q p >>= pen plain
  l <- ray r q >>= pen green
  m <- line r p >>= pen red
  n <- line p q >>= pen blue
  return ()

{-
fig2 :: Fig PoincarePlane ()
fig2 = do
  p <- coords 1 2
  q <- coords 5 3
  r <- coords 1 2
  s <- coords 4 2
  t <- intersectLines (p,q) (r,s) >>= pen plain
  return ()
-}

writeEPS' :: (ToCartesian t) => String -> Fig t a -> IO ()
writeEPS' name fig = do
  let
    com = case view fig of
      Left err -> []
      Right cs -> cs
  let x = writeFormat EPS viewOptsUnitDisc com
  writeFile name x


writeEPS :: (ToCartesian t) => String -> Fig t a -> IO ()
writeEPS name fig = do
  let
    com = case view fig of
      Left err -> []
      Right cs -> cs
  let x = writeFormat EPS vOpts com
  writeFile name x

testAnim :: IO ()
testAnim = animate pic2 aOpts viewOptsUnitDisc EPS "ani"


pic :: ConReal -> Fig PoincarePlane ()
pic t = do
  let p' = BezierPath [(10,10), (0,5), (-6,7), (5,4)]
  let p  = H $ path p' t
  let q' = BezierPath [(-10,2), (7,-2), (-5,8), (1,9)]
  let q  = H $ path q' t
  let r' = BezierPath [(1,-7), (-9,-8), (5,0)]
  let r  = H $ path r' t
  circle p q >>= pen blue
  circle r p >>= pen red
  circle q r >>= pen green
  pen plain p
  pen plain q
  pen plain r
  return ()

pic2 :: ConReal -> Fig (PoincareDisc ConReal) ()
pic2 t = do
  let p' = CirclePath (0,0) (1/2) 0 360
  let Just p = mkPoincareDisc $ path p' t
  let q' = CirclePath (1/2,0) (1/3) 0 720
  let Just q = mkPoincareDisc $ path q' t
  let r' = CirclePath (0,-1/3) (1/4) 0 360
  let Just r = mkPoincareDisc $ path r' t
  circle p q >>= pen blue
  circle r p >>= pen red
  circle q r >>= pen green
  pen plain p
  pen plain q
  pen plain r
  return ()


-}
