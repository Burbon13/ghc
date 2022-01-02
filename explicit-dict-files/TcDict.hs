{-# LANGUAGE DuplicateRecordFields #-}

module TcDict where

data Eq.MyDict a = Eq.MyDict {
  eda_eq :: a -> a -> Bool,
  eda_neq :: a -> a -> Bool
}

myInstDict :: Eq.MyDict String
myInstDict = Eq.MyDict {
   eda_eq = \x y -> True,
   eda_neq = \x y -> False
}

f :: Eq a => a -> a -> Int
f x y = if x == y then 1 else 0

test = f ((myInstDict)) "aa" "cc"
