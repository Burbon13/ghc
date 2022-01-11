{-  TEST - SHOULD COMPILE
Dictionary Exposure of locally defined type classes.
-}

module DictExpose01 where

class Train a where
  choo_choo :: a -> String

f1 :: Train.Dict -> Int -> Int
f1 x y = y

f2 :: Train.Dict -> Train.Dict
f2 x = x

f3 :: (Train.Dict -> Int) -> Train.Dict -> Int
f3 f x = f x

f4 :: (Train.Dict -> Train.Dict) -> Train.Dict -> Train.Dict
f4 f t = f t
