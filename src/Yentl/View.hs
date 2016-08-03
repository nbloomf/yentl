module Yentl.View (
  Format(..), writeFormat,
  animate, ToCartesian,
  module Yentl.View.Options
) where

import Yentl.Algebra
import Yentl.Geo
import Yentl.View.Command
import Yentl.View.Cartesian
import Yentl.View.PostScript
import Yentl.View.Format
import Yentl.View.Options

writeFormat :: (ToCartesian t) => Format -> ViewOpts -> [Command t] -> String
writeFormat format opts xs = case format of
  EPS -> writePostScript opts xs


animate :: (ToCartesian t, Show t)
  => (Rational -> Fig t a) -> AnimateOpts -> ViewOpts -> FilePath -> String -> IO ()
animate fig aOpts vOpts path name = do
  let
    (lo,hi,num) = (optStartT aOpts, optEndT aOpts, optNumFrames aOpts)
    dt = fromRational $ (hi - lo)/(fromIntegral num)
    frame k = do
      pic <- viewIO $ fig (lo + (fromIntegral k)*dt)
      let file = writeFormat EPS vOpts pic
      writeFile (path ++ name ++ "-" ++ pad num k ++ ".eps") file
  sequence_ $ map frame [0..num]


pad :: Int -> Int -> String
pad n k = reverse $ zipWith (const id) (show n) ((reverse $ show k) ++ (repeat '0'))
