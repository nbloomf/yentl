module Yentl.Verify where


exactlyOne :: [Bool] -> Bool
exactlyOne []  = False
exactlyOne [p] = p
exactlyOne (p:ps) = case p of
  True  -> and $ map not ps
  False -> exactlyOne ps

data If = If [Bool]
data Then = Then [Bool]

testGeoProperty :: String -> (If,Then) -> Bool
testGeoProperty name (If condition, Then props) =
  let
    msg = concat
      [ "error running test " ++ name ++ ": precondition not satisfied.\n"
      , "perhaps you need to use a different test case generator.\n"
      ] in
  if not $ and condition
    then error msg
    else and props

