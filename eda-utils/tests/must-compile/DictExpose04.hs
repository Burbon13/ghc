{-  TEST - SHOULD COMPILE and RUN
Dictionary Exposure of locally defined type classes.
-}

module DictExpose04 where

class Train a where
  choo_choo :: String
  transform :: Transport a => a -> a

class Transport a where
  transformTrain :: a -> Train.Dict -> Train.Dict
  printTrain :: a -> Train.Dict -> String

