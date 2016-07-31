{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yentl.Algebra.Equation.Quadratic (
  QuadraticEquation(), qeCoefs, qeFromCoefs, qeDiscriminant
) where

import Yentl.Algebra.Classes
import Yentl.Algebra.Data.AtMost
import Yentl.Algebra.Number.Sign
import Yentl.Algebra.Equation.Solution
import Yentl.Algebra.Equation.System2



{---------------------}
{- QuadraticEquation -}
{---------------------}

-- ax^2 + bx + c = 0
data QuadraticEquation a = QE
  { qeACoef :: a
  , qeBCoef :: a
  , qeCCoef :: a
  } deriving Eq


qeCoefs :: QuadraticEquation a -> (a,a,a)
qeCoefs x = (qeACoef x, qeBCoef x, qeCCoef x)


-- safe constructor
qeFromCoefs :: (Eq a, Num a)
  => a -> a -> a -> Maybe (QuadraticEquation a)
qeFromCoefs a b c = if a == 0
  then Nothing
  else Just $ QE
    { qeACoef = a
    , qeBCoef = b
    , qeCCoef = c
    }


qeDiscriminant :: (Num a) => QuadraticEquation a -> a
qeDiscriminant eq =
  let (a,b,c) = qeCoefs eq in b*b - 4*a*c



{-------------}
{- Instances -}
{-------------}

instance (Eq a, Num a) => EquationIn1Var a (QuadraticEquation a) where
  isSolutionOf1 x eq =
    let (a,b,c) = qeCoefs eq in
    a*x^2 + b*x + c == 0


{-----------}
{- Systems -}
{-----------}

instance (Ord a, Num a, Fractional a, SquareRoot a)
  => Solve1var2sol a (QuadraticEquation a) where
  solve1var2sol eq =
    let (a,b,c) = qeCoefs eq in
    if a==0
      then error "qeSolution"
      else
        let disc = qeDiscriminant eq in
        case signOf disc of
          Negative -> Only0of2
          Zero ->
            let x = (negate b)/(2*a) in
            Only1of2 x
          Positive ->
            case root2 disc of
              Nothing -> error ""
              Just q ->
                let x1 = (negate b + q)/(2*a) in
                let x2 = (negate b - q)/(2*a) in
                Only2of2 x1 x2
