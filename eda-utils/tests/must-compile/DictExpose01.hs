{-  TEST - SHOULD COMPILE
Dictionary Exposure of locally defined type classes.
-}

module DictExpose01 where

class Train a where
  choo_choo :: a -> String

f1 :: Train__Dict -> Int -> Int
f1 x y = y

f2 :: Train__Dict -> Train__Dict
f2 x = x

f3 :: (Train__Dict -> Int) -> Train__Dict -> Int
f3 f x = f x

f4 :: (Train__Dict -> Train__Dict) -> Train__Dict -> Train__Dict
f4 f t = f t
