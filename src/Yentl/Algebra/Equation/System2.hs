{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Yentl.Algebra.Equation.System2 (
  Solve1var2sol, Solve2var1sol, Solve2var2sol,
  solve1var2sol, solve2var1sol, solve2var2sol
) where

import Yentl.Algebra.Data.AtMost


{------------------------------------------}
{- real solutions of systems of equations -}
{------------------------------------------}

-- 1 variable, at most 2 solutions
class Solve1var2sol a t | t -> a where
  solve1var2sol :: t -> AtMost2 a


-- 2 variables, at most 1 solution
class Solve2var1sol a t | t -> a where
  solve2var1sol :: t -> Maybe (a,a)


-- 2 variables, at most 2 solutions
class Solve2var2sol a t | t -> a where
  solve2var2sol :: t -> AtMost2 (a,a)
