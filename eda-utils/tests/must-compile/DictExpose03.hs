{-  TEST - SHOULD COMPILE and RUN
Dictionary Exposure of locally defined type classes.
-}

module DictExpose03 where

class Train a where
  choo_choo :: a -> String

class Transport a where
  transformTrain :: a -> Train.Dict -> Train.Dict
  printTrain :: a -> Train.Dict -> String

instance Transport Int where
  transformTrain a td = td
  printTrain a td = "TRains!"
