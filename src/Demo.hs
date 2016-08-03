module Demo (demo) where

import Yentl
import System.Directory


demoImg :: (Show t, ToCartesian t) =>
  Fig t a -> Format -> ViewOpts -> FilePath -> String -> String -> IO ()
demoImg fig format opts path name desc = do
  createDirectoryIfMissing True path
  xs <- viewIO fig
  writeFile (path ++ name) $ writeFormat format opts xs
  writeFile (path ++ "readme.txt") desc

demoAni :: (Show t, ToCartesian t) =>
  (Rational -> Fig t a) -> ViewOpts -> AnimateOpts -> FilePath -> String -> String -> IO ()
demoAni fig vOpts aOpts path name desc = do
  createDirectoryIfMissing True path
  animate fig aOpts vOpts path name
  writeFile (path ++ "readme.txt") desc
  let make =
        "all:\n" ++
        "\tfor i in `seq -w 0 " ++ show 30 ++ "`; \\\n" ++
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
        p <- coords $ path (CirclePath (0,0) 8 0 360) t
        pen plain p
        q <- coords (0,0) >>= pen plain
        segment p q >>= pen plain
        return ()

      vOpts = defaultView
      aOpts = defaultAnimate
    in
      demoAni fig vOpts aOpts "demo/002/" "002"
        "Animation of a rotating segment in the cartesian plane.\n"
  ]
