{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Yentl.Algebra.Equation.Solution (
  EquationIn1Var, isSolutionOf1,
  EquationIn2Var, isSolutionOf2
) where


class EquationIn1Var a t | t -> a where
  isSolutionOf1 :: a -> t -> Bool


class EquationIn2Var a t | t -> a where
  isSolutionOf2 :: (a,a) -> t -> Bool
