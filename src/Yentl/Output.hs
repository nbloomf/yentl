module Yentl.Output (
  writeImg, writeAni
) where

import Yentl.View
import Yentl.Geo
import System.Directory

writeImg :: (Show t, ToCartesian t) =>
  Fig t a -> Format -> ViewOpts -> FilePath -> String -> String -> IO ()
writeImg fig format opts path name desc = do
  putStrLn $ "building " ++ path ++ name
  createDirectoryIfMissing True path
  xs <- viewIO fig
  writeFile (path ++ name) $ writeFormat format opts xs
  writeFile (path ++ "readme.txt") desc
  putStrLn $ "built " ++ path ++ name

writeAni :: (Show t, ToCartesian t) =>
  (Rational -> Fig t a) -> ViewOpts -> AnimateOpts -> FilePath -> String -> String -> IO ()
writeAni fig vOpts aOpts path name desc = do
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
  putStrLn $ "built " ++ path ++ name
