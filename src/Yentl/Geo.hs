module Yentl.Geo (
  Fig, view, report, assert, pen, isValid, execute,

  Coords, coords,

  module Yentl.Error,
  module Yentl.View.Command
) where

import Yentl.View.Command
import Yentl.Error

data Geo u v a = Geo { runGeo :: (Either v a, [u]) }


instance Monad (Geo u v) where
  return x = Geo {runGeo = (Right x, [])}

  (Geo (Left e, w))  >>= _ = Geo (Left e, w)

  (Geo (Right x, w)) >>= f =
    let (y,v) = runGeo (f x)
    in Geo (y, w++v)

execute :: Geo u v a -> Maybe a
execute x = case runGeo x of
  (Right a, _) -> Just a
  _            -> Nothing

instance (Show a, Show u, Show v) => Show (Geo u v a) where
  show fig = case runGeo fig of
    (Right a, u) -> show a ++ "\n" ++ show u
    (Left v, u) -> show v ++ "\n" ++ show u


-- add to list of drawing instructions
write :: [u] -> Geo u v ()
write us = Geo {runGeo = (Right (), us)}

-- get list of drawing instructions
view :: Geo u v a -> Either v [u]
view x = case runGeo x of
  (Left e, _)  -> Left e
  (Right _, w) -> Right w

quietly :: Geo u v a -> Geo u v a
quietly x = case runGeo x of
  (Left e, _)  -> report e
  (Right y, _) -> return y


-- report error
report :: v -> Geo u v t
report err = Geo {runGeo = (Left err, [])}


type Fig t = Geo (Command t) (GeoError t)

-- check for failure
isValid :: Fig t a -> Bool
isValid x = case runGeo x of
  (Left _, _) -> False
  (Right _, _) -> True

assert :: Bool -> String -> Fig t ()
assert True  _   = return ()
assert False msg = report $ AssertionFailure msg

pen :: (Draw t a) => Style -> a -> Fig t a
pen sty x = do
  write $ draw x <% sty
  return x

class Coords t where
  coords :: Rational -> Rational -> Fig t t



