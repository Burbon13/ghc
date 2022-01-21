{-  TEST - SHOULD COMPILE and RUN
Dictionary Exposure of locally defined type classes.
-}

module DictExpose04 where

class Train a b c where
  choo_choo :: b -> c -> a -> String
  add_dist :: a -> c -> b -> b -> Int
  routes :: b -> a -> a -> c

type X = Train__Dict Int String Int
newtype Y = Train__Dict Bool
