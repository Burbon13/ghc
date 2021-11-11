module SimpleCase where

f :: Eq a => a -> a -> Bool
f v1 v2 = v1 == v2

-- f = testEq ((aaa)) "haskell" "Haskell"
x = f ((dict)) "aaa" "Aaa"