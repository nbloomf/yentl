module Test.Verify where


exactlyOne :: [Bool] -> Bool
exactlyOne []  = False
exactlyOne [p] = p
exactlyOne (p:ps) = case p of
  True  -> and $ map not ps
  False -> exactlyOne ps

data If = If [Bool]
data Then = Then [Bool]

verify :: (If,Then) -> Bool
verify (If condition, Then props) =
  if not $ and condition
    then error "condition not met"
    else and props
