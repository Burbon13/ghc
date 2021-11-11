module SimpleCaseDictApp where

testEq :: Eq a => a -> a -> Bool
testEq v1 v2 = v1 == v2

data EqDict = EqDict {(==) :: String -> String -> Bool}

eqOn :: (String -> String) -> EqDict
eqOn f = EqDict { (==) = (\v1 v2 -> f v1 == f v2) }

f = testEq ((eqOn (\s -> s))) "haskell" "Haskell"
