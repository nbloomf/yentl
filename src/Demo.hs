module Demo (demo) where

import Yentl
import System.Directory
import Control.Concurrent
import Control.Monad (forever)
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Yentl.Output


writer :: Chan String -> IO ()
writer chan = do
  forever $ do
    threadDelay 500
    gossip <- readChan chan
    putStrLn gossip


demoImg :: (Show t, ToCartesian t) =>
  Chan String -> Fig t a -> Format -> ViewOpts -> FilePath -> String -> String -> IO ()
demoImg chan fig format opts path name desc = do
  writeChan chan $ "building " ++ path ++ name
  createDirectoryIfMissing True path
  xs <- viewIO fig
  writeFile (path ++ name) $ writeFormat format opts xs
  writeFile (path ++ "readme.txt") desc
  writeChan chan $ "built " ++ path ++ name

demoAni :: (Show t, ToCartesian t) =>
  Chan String -> (Rational -> Fig t a) -> ViewOpts -> AnimateOpts -> FilePath -> String -> String -> IO ()
demoAni chan fig vOpts aOpts path name desc = do
  writeChan chan $ "building " ++ path ++ name
  createDirectoryIfMissing True path
  animate fig aOpts vOpts path name
  writeFile (path ++ "readme.txt") desc
  let num = optNumFrames aOpts
  let make =
        "all:\n" ++
        "\tfor i in `seq -w 0 " ++ show num ++ "`; \\\n" ++
        "\t  do convert " ++ name ++ "-$$i.eps -resize 400x400 -depth 4 -strip " ++ name ++ "-$$i.png; \\\n" ++
        "\t  done\n" ++
        "\tconvert -dispose previous " ++ name ++ "*.png " ++ name ++ ".gif"
  writeFile (path ++ "makefile") make
  writeChan chan $ "built " ++ path ++ name


demo :: IO ()
demo = do
  chan <- newChan
  forkIO $ writer chan
  mapConcurrently ($ chan)
    [ demo001
    , demo002
    , demo003
    , demo004
    , demo005
    , demo006
    , demo007
    , demo008
    , demo009
    ]
  return ()


demo001 :: Chan String -> IO ()
demo001 chan = do
  let
    fig :: Fig CartesianPlane ()
    fig = do
      p <- coords (0,0) >>= pen plain
      q <- coords (1,0) >>= pen plain
      r <- coords (0,1) >>= pen plain
      segment p q >>= pen plain
      segment q r >>= pen plain
      segment r p >>= pen plain
      return ()

    vOpts = bestFit 1 defaultView
  demoImg chan fig EPS vOpts "demo/001/" "001.eps" $
    "Drawing of a triangle in the cartesian plane.\n"


demo002 :: Chan String -> IO ()
demo002 chan = do
  let
    fig :: Rational -> Fig CartesianPlane ()
    fig t = do
      p <- coords $ path (CirclePath (0,0) 8 0 1) t
      pen plain p
      q <- coords (0,0) >>= pen plain
      segment p q >>= pen plain
      return ()

    vOpts = defaultView
    aOpts = defaultAnimate
  demoAni chan fig vOpts aOpts "demo/002/" "002" $
    "Animation of a rotating segment in the cartesian plane.\n"


demo003 :: Chan String -> IO ()
demo003 chan = do
  let
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
  demoImg chan fig EPS vOpts "demo/003/" "003.eps" $
    "Drawing of a triangle in the poincare plane.\n"


demo004 :: Chan String -> IO ()
demo004 chan = do
  let
    fig :: Rational -> Fig PoincarePlane ()
    fig t = do
      p <- coords $ path (CirclePath (0,11/2) 5 0 1) t
      pen plain p
      q <- coords (0,3) >>= pen plain
      segment p q >>= pen plain
      return ()

    vOpts = fixed ((-6,-1),(6,12)) defaultView
    aOpts = frames 50 defaultAnimate
  demoAni chan fig vOpts aOpts "demo/004/" "004" $
    "Animation of a rotating segment in the poincare plane.\n"


demo005 :: Chan String -> IO ()
demo005 chan = do
  let
    fig :: Rational -> Fig PoincarePlane ()
    fig t = do
      p1 <- coords $ path (CirclePath (0,11/2) 5 (0/7) 1) t
      p2 <- coords $ path (CirclePath (0,11/2) 5 (1/7) 1) t
      p3 <- coords $ path (CirclePath (0,11/2) 5 (2/7) 1) t
      p4 <- coords $ path (CirclePath (0,11/2) 5 (3/7) 1) t
      p5 <- coords $ path (CirclePath (0,11/2) 5 (4/7) 1) t
      p6 <- coords $ path (CirclePath (0,11/2) 5 (5/7) 1) t
      p7 <- coords $ path (CirclePath (0,11/2) 5 (6/7) 1) t
      let ps = [p1,p2,p3,p4,p5,p6,p7]
      segment p1 p3 >>= pen red
      segment p2 p4 >>= pen blue
      segment p3 p5 >>= pen green
      segment p4 p6 >>= pen red
      segment p5 p7 >>= pen blue
      segment p6 p1 >>= pen green
      segment p7 p2 >>= pen plain
      sequence $ map (pen plain) ps
      return ()

    vOpts = fixed ((-6,-1),(6,12)) defaultView
    aOpts = frames 100 defaultAnimate
  demoAni chan fig vOpts aOpts "demo/005/" "005" $
    "Animation of a rotating figure in the poincare plane.\n"


demo006 :: Chan String -> IO ()
demo006 chan = do
  let
    fig :: Rational -> Fig CartesianPlane ()
    fig t = do
      a <- coords $ path (CirclePath (0,0) 4    0  1) t
      o <- coords $ path (CirclePath (0,0) 3 (1/2) 2) t
      if a == o
        then do
          pen plain o
          return ()
        else do
          x <- pointBefore o a
          c <- circle o a >>= pen plain
          let Just b = cutCircleRay (o,a) x
          r <- ray o x >>= pen plain
          pen plain x
          pen red o
          pen blue a
          pen green b
          return ()

    vOpts = fixed ((-20,-20),(20,20)) defaultView
    aOpts = frames 100 defaultAnimate
  demoAni chan fig vOpts aOpts "demo/006/" "006" $
    "Construction of an antipode in the cartesian plane.\n" ++
    "(Antipode of blue through red at green.)\n"


demo007 :: Chan String -> IO ()
demo007 chan = do
  let
    fig :: Rational -> Fig PoincarePlane ()
    fig t = do
      a <- coords $ path (CirclePath (0,5) 4    0  1) t
      o <- coords $ path (CirclePath (0,5) 3 (1/2) 2) t
      if a == o
        then do
          pen plain o
          return ()
        else do
          x <- pointBefore o a
          c <- circle o a >>= pen plain
          let Just b = cutCircleRay (o,a) x
          r <- ray o x >>= pen plain
          pen plain x
          pen red o
          pen blue a
          pen green b
          return ()

    vOpts = fixed ((-6,-1),(6,12)) defaultView
    aOpts = frames 100 defaultAnimate
  demoAni chan fig vOpts aOpts "demo/007/" "007" $
    "Construction of an antipode in the poincare plane.\n" ++
    "(Antipode of blue through red at green.)\n"


demo008 :: Chan String -> IO ()
demo008 chan = do
  let
    fig :: Rational -> Fig (PoincareDisc ConReal) ()
    fig t = do
      a <- coords $ path (CirclePath (0,0) (1/2)    0  1) t
      o <- coords $ path (CirclePath (0,0) (1/3) (1/2) 2) t
      if a == o
        then do
          pen plain o
          return ()
        else do
          x <- pointBefore o a
          c <- circle o a >>= pen plain
          let Just b = cutCircleRay (o,a) x
          r <- ray o x >>= pen plain
          pen plain x
          pen red o
          pen blue a
          pen green b
          return ()

    vOpts = discView
    aOpts = frames 100 defaultAnimate
  demoAni chan fig vOpts aOpts "demo/008/" "008" $
    "Construction of an antipode in the poincare disc.\n" ++
    "(Antipode of blue through red at green.)\n"


demo009 :: Chan String -> IO ()
demo009 chan = do
  let
    fig :: Rational -> Fig (PoincareDisc ConReal) ()
    fig t = do
      let c1 = path (CirclePath (0,0) (2/3) 0 (-1)) t
      p1 <- coords $ path (CirclePath c1 (2/9) (0/3) 2) t
      p2 <- coords $ path (CirclePath c1 (2/9) (1/3) 2) t
      p3 <- coords $ path (CirclePath c1 (2/9) (2/3) 2) t
      let ps = [p1,p2,p3]
      segment p1 p2 >>= pen red
      segment p2 p3 >>= pen blue
      segment p3 p1 >>= pen green
      sequence $ map (pen plain) ps

      let c2 = path (CirclePath (0,0) (2/3) (1/3) (-1)) t
      q1 <- coords $ path (CirclePath c2 (2/9) (1/3) 2) t
      q2 <- coords $ path (CirclePath c2 (2/9) (2/3) 2) t
      q3 <- coords $ path (CirclePath c2 (2/9) (0/3) 2) t
      let qs = [q1,q2,q3]
      segment q1 q2 >>= pen red
      segment q2 q3 >>= pen blue
      segment q3 q1 >>= pen green
      sequence $ map (pen plain) qs

      let c3 = path (CirclePath (0,0) (2/3) (2/3) (-1)) t
      r1 <- coords $ path (CirclePath c3 (2/9) (2/3) 2) t
      r2 <- coords $ path (CirclePath c3 (2/9) (0/3) 2) t
      r3 <- coords $ path (CirclePath c3 (2/9) (1/3) 2) t
      let rs = [r1,r2,r3]
      segment r1 r2 >>= pen red
      segment r2 r3 >>= pen blue
      segment r3 r1 >>= pen green
      sequence $ map (pen plain) rs
      return ()

    vOpts = discView
    aOpts = frames 100 defaultAnimate
  demoAni chan fig vOpts aOpts "demo/009/" "009" $
    "Rotating triangles in the poincare disc.\n"

