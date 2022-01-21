{-  TEST - SHOULD COMPILE
Dictionary Exposure of locally defined type classes.
-}

module DictExpose01 where

class Train a b where
  choo_choo :: a -> b -> String

f1 :: Train__Dict a b -> Int -> Int
f1 x y = y

f2 :: (Train__Dict Int Int -> Train__Dict Bool Bool)
      -> Train__Dict Int Int
      -> Train__Dict Bool Bool
f2 ftrain train1 = ftrain train1
