module Demo (demo) where

import Yentl
import System.Directory


demoImg :: (Show t, ToCartesian t) =>
  Fig t a -> Format -> ViewOpts -> FilePath -> String -> String -> IO ()
demoImg fig format opts path name desc = do
  putStrLn $ "building " ++ path ++ name
  createDirectoryIfMissing True path
  xs <- viewIO fig
  writeFile (path ++ name) $ writeFormat format opts xs
  writeFile (path ++ "readme.txt") desc

demoAni :: (Show t, ToCartesian t) =>
  (Rational -> Fig t a) -> ViewOpts -> AnimateOpts -> FilePath -> String -> String -> IO ()
demoAni fig vOpts aOpts path name desc = do
  putStrLn $ "building " ++ path ++ name
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


demo :: IO ()
demo = sequence_
  [ let
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
    in
      demoImg fig EPS vOpts "demo/001/" "001.eps"
        "Drawing of a triangle in the cartesian plane.\n"


  , let
      fig :: Rational -> Fig CartesianPlane ()
      fig t = do
        p <- coords $ path (CirclePath (0,0) 8 0 1) t
        pen plain p
        q <- coords (0,0) >>= pen plain
        segment p q >>= pen plain
        return ()

      vOpts = defaultView
      aOpts = defaultAnimate
    in
      demoAni fig vOpts aOpts "demo/002/" "002"
        "Animation of a rotating segment in the cartesian plane.\n"


  , let
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
    in
      demoImg fig EPS vOpts "demo/003/" "003.eps"
        "Drawing of a triangle in the poincare plane.\n"


  , let
      fig :: Rational -> Fig PoincarePlane ()
      fig t = do
        p <- coords $ path (CirclePath (0,11/2) 5 0 1) t
        pen plain p
        q <- coords (0,3) >>= pen plain
        segment p q >>= pen plain
        return ()

      vOpts = fixed ((-6,-1),(6,12)) defaultView
      aOpts = frames 50 defaultAnimate
    in
      demoAni fig vOpts aOpts "demo/004/" "004"
        "Animation of a rotating segment in the poincare plane.\n"


  , let
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
    in
      demoAni fig vOpts aOpts "demo/005/" "005"
        "Animation of a rotating figure in the poincare plane.\n"
  ]
