{-# LANGUAGE MultiParamTypeClasses #-}

module Yentl.Algebra.Data.AtMost where

import Data.List (nub)


{----------}
{- AtMost -}
{----------}

class AtMost t where
  toList   :: t a -> [a]
  fromList :: [a] -> Maybe (t a)

class InjectAtMost t u where
  injectAtMost :: t a -> u a


mapAtMost :: (AtMost t) => (a -> b) -> t a -> t b
mapAtMost f x =
  let Just y = fromList $ map f $ toList x in y


filterAtMost :: (AtMost t) => (a -> Bool) -> t a -> t a
filterAtMost p x =
  let Just y = fromList $ filter p $ toList x in y


nubAtMost :: (AtMost t, Eq a) => t a -> t a
nubAtMost x =
  let Just y = fromList $ nub $ toList x in y



{-----------}
{- AtMost2 -}
{-----------}

data AtMost2 a
  = Only0of2
  | Only1of2 a
  | Only2of2 a a
  deriving (Eq, Show)



instance AtMost AtMost2 where
  toList m = case m of
    Only0of2     -> []
    Only1of2 x   -> [x]
    Only2of2 x y -> [x,y]

  fromList m = case m of
    []    -> Just $ Only0of2
    [x]   -> Just $ Only1of2 x
    [x,y] -> Just $ Only2of2 x y
    _     -> Nothing

instance InjectAtMost Maybe AtMost2 where
  injectAtMost m = case m of
    Nothing -> Only0of2
    Just x  -> Only1of2 x



{-----------}
{- AtMost3 -}
{-----------}

data AtMost3 a
  = Only0of3
  | Only1of3 a
  | Only2of3 a a
  | Only3of3 a a a
  deriving (Eq, Show)


instance AtMost AtMost3 where
  toList m = case m of
    Only0of3       -> []
    Only1of3 x     -> [x]
    Only2of3 x y   -> [x,y]
    Only3of3 x y z -> [x,y,z]

  fromList m = case m of
    []      -> Just $ Only0of3
    [x]     -> Just $ Only1of3 x
    [x,y]   -> Just $ Only2of3 x y
    [x,y,z] -> Just $ Only3of3 x y z
    _       -> Nothing


instance InjectAtMost Maybe AtMost3 where
  injectAtMost m = case m of
    Nothing -> Only0of3
    Just x  -> Only1of3 x


instance InjectAtMost AtMost2 AtMost3 where
  injectAtMost m = case m of
    Only0of2     -> Only0of3
    Only1of2 x   -> Only1of3 x
    Only2of2 x y -> Only2of3 x y



{-----------}
{- AtMost4 -}
{-----------}

data AtMost4 a
  = Only0of4
  | Only1of4 a
  | Only2of4 a a
  | Only3of4 a a a
  | Only4of4 a a a a
  deriving (Eq, Show)


instance AtMost AtMost4 where
  toList m = case m of
    Only0of4         -> []
    Only1of4 x       -> [x]
    Only2of4 x y     -> [x,y]
    Only3of4 x y z   -> [x,y,z]
    Only4of4 x y z w -> [x,y,z,w]

  fromList m = case m of
    []        -> Just $ Only0of4
    [x]       -> Just $ Only1of4 x
    [x,y]     -> Just $ Only2of4 x y
    [x,y,z]   -> Just $ Only3of4 x y z
    [x,y,z,w] -> Just $ Only4of4 x y z w
    _         -> Nothing
