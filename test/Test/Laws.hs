module Test.Laws (
  testIdemp,
  testAssoc,
  testCommu,
  testDistL,
  testDistR,
  testNeutL,
  testNeutR,

  testNeg,
  testNegOneL,
  testNegOneR,
  testNegProdL,
  testNegProdR,
  testValue,
  testHom
) where

import Test.QuickCheck


-- unary idempotence
testIdemp :: (Eq t, Arbitrary t, Show t)
  => (t -> t) -> Property
testIdemp op = forAll arbitrary
  (\a -> op (op a) == a)


-- associativity
testAssoc :: (Eq t, Arbitrary t, Show t)
  => (t -> t -> t) -> Property
testAssoc op = forAll arbitrary
  (\(a,b,c) -> op (op a b) c == op a (op b c))


-- binary commutativity
testCommu :: (Eq u, Arbitrary t, Show t)
  => (t -> t -> u) -> Property
testCommu op = forAll arbitrary
  (\(a,b) -> op a b == op b a)


-- binary left distributivity
testDistL :: (Eq v, Arbitrary t, Show t, Arbitrary u, Show u, Arbitrary v, Show v)
  => (u -> t -> v) -> (t -> t -> t) -> (v -> v -> v) -> Property
testDistL op1 op2 op3 = forAll arbitrary
  (\(a,b,c) -> op1 a (op2 b c) == op3 (op1 a b) (op1 a c))


-- binary right distributivity
testDistR :: (Eq v, Arbitrary t, Show t, Arbitrary u, Show u, Arbitrary v, Show v)
  => (u -> t -> v) -> (u -> u -> u) -> (v -> v -> v) -> Property
testDistR op1 op2 op3 = forAll arbitrary
  (\(a,b,c) -> op1 (op2 a b) c == op3 (op1 a c) (op1 b c))


-- left neutrality
testNeutL :: (Eq t, Arbitrary t, Show t)
  => (u -> t -> t) -> u -> Property
testNeutL op e = forAll arbitrary
  (\a -> op e a == a)


-- right neutrality
testNeutR :: (Eq t, Arbitrary t, Show t)
  => (t -> u -> t) -> u -> Property
testNeutR op e = forAll arbitrary
  (\a -> op a e == a)


testNeg :: (Eq t, Arbitrary t, Show t)
  => (t -> t -> t) -> (t -> t) -> t -> Property
testNeg op inv e = forAll arbitrary
  (\a -> op a (inv a) == e && op (inv a) a == e)


testNegOneL :: (Eq t, Arbitrary t, Show t)
  => (u -> t -> t) -> (u -> u) -> (t -> t) -> u -> Property
testNegOneL op neg1 neg2 one = forAll arbitrary
  (\a -> op (neg1 one) a == neg2 a)


testNegOneR :: (Eq t, Arbitrary t, Show t)
  => (t -> u -> t) -> (u -> u) -> (t -> t) -> u -> Property
testNegOneR op neg1 neg2 one = forAll arbitrary
  (\a -> op a (neg1 one) == neg2 a)


testNegProdL :: (Eq v, Arbitrary t, Show t, Arbitrary u, Show u)
  => (t -> u -> v) -> (t -> t) -> (v -> v) -> Property
testNegProdL op op1 op2 = forAll arbitrary
  (\(a,b) -> op (op1 a) b == op2 (op a b))


testNegProdR :: (Eq v, Arbitrary t, Show t, Arbitrary u, Show u)
  => (t -> u -> v) -> (u -> u) -> (v -> v) -> Property
testNegProdR op op1 op2 = forAll arbitrary
  (\(a,b) -> op a (op1 b) == op2 (op a b))


testValue :: (Eq u)
  => (t -> u) -> t -> u -> Bool
testValue phi t u = phi t == u


testHom :: (Eq u, Arbitrary t, Show t)
  => (t -> u) -> (t -> t -> t) -> (u -> u -> u) -> Property
testHom phi op1 op2 = forAll arbitrary
  (\(a,b) -> phi (op1 a b) == op2 (phi a) (phi b))
