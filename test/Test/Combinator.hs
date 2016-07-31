module Test.Combinator (
  arbIn01, arbInInterval,
  arb,
  tuple2,  tuple3,  tuple4,
  spawn2,  spawn3,
  diff2,   diff3,   diff4,
  sorted2, sorted3, sorted4
) where

import Test.QuickCheck


-- arbitrary in (0,1)
arbIn01 :: (Arbitrary a, Ord a, Num a, Fractional a)
  => Gen a
arbIn01 = do
  Positive x <- arbitrary
  Positive y <- arbitrary
  let z = x/(x + y)
  let True = 0 <= z && z < 1
  return z

-- arbitrary in (lo,hi)
arbInInterval :: (Arbitrary a, Ord a, Num a, Fractional a)
  => a -> a -> Gen a
arbInInterval a b = do
  let lo = min a b
  let hi = max a b
  t <- arbIn01
  let c = a + t*(b-a)
  let True = a <= c && c < b
  return c




{-------------------------}
{- Generator Combinators -}
{-------------------------}

arb :: (Arbitrary a) => a -> Gen a
arb _ = arbitrary



{- Tuples -}

tuple2 :: Gen a -> Gen b -> Gen (a,b)
tuple2 genA genB = do
  x <- genA
  y <- genB
  return (x,y)


tuple3 :: Gen a -> Gen b -> Gen c -> Gen (a,b,c)
tuple3 genA genB genC = do
  x <- genA
  y <- genB
  z <- genC
  return (x,y,z)


tuple4 :: Gen a -> Gen b -> Gen c -> Gen d -> Gen (a,b,c,d)
tuple4 genA genB genC genD = do
  x <- genA
  y <- genB
  z <- genC
  w <- genD
  return (x,y,z,w)



{- Tuples (Possibly Equal) -}

spawn2 :: Gen a -> Gen (a,a)
spawn2 gen = tuple2 gen gen


spawn3 :: Gen a -> Gen (a,a,a)
spawn3 gen = tuple3 gen gen gen


spawn4 :: Gen a -> Gen (a,a,a,a)
spawn4 gen = tuple4 gen gen gen gen



{- Tuples (Distinct) -}

diff2 :: (Eq a) => Gen a -> Gen (a,a)
diff2 gen = do
  a <- gen
  b <- gen `suchThat` (\x -> x /= a)
  return (a,b)


diff3 :: (Eq a) => Gen a -> Gen (a,a,a)
diff3 gen = do
  a <- gen
  b <- gen `suchThat` (\x -> x /= a)
  c <- gen `suchThat` (\x -> x /= a && x /= b)
  return (a,b,c)


diff4 :: (Eq a) => Gen a -> Gen (a,a,a,a)
diff4 gen = do
  a <- gen
  b <- gen `suchThat` (\x -> x /= a)
  c <- gen `suchThat` (\x -> x /= a && x /= b)
  d <- gen `suchThat` (\x -> x /= a && x /= b && x /= c)
  return (a,b,c,d)



{- Tuples (Distinct & Ordered) -}

sorted2 :: (Ord a, Arbitrary a) => Gen a -> Gen (a,a)
sorted2 gen = fmap sort2 $ diff2 gen


sorted3 :: (Ord a, Arbitrary a) => Gen a -> Gen (a,a,a)
sorted3 gen = fmap sort3 $ diff3 gen


sorted4 :: (Ord a, Arbitrary a) => Gen a -> Gen (a,a,a,a)
sorted4 gen = fmap sort4 $ diff4 gen



{------------------}
{- Sorting Tuples -}
{------------------}

insert2 :: (Ord a) => a -> a -> (a,a)
insert2 x a = if x <= a
  then (x,a)
  else (a,x)

sort2 :: (Ord a) => (a,a) -> (a,a)
sort2 (a,b) = insert2 b a

insert3 :: (Ord a) => a -> (a,a) -> (a,a,a)
insert3 x (a,b) = if x <= a
  then (x,a,b)
  else if x <= b
    then (a,x,b)
    else (a,b,x)

sort3 :: (Ord a) => (a,a,a) -> (a,a,a)
sort3 (a,b,c) = insert3 c $ insert2 b a

insert4 :: (Ord a) => a -> (a,a,a) -> (a,a,a,a)
insert4 x (a,b,c) = if x <= a
  then (x,a,b,c)
  else if x <= b
    then (a,x,b,c)
    else if x <= c
      then (a,b,x,c)
      else (a,b,c,x)

sort4 :: (Ord a) => (a,a,a,a) -> (a,a,a,a)
sort4 (a,b,c,d) = insert4 d $ insert3 c $ insert2 b a
