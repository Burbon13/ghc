{-  TEST - SHOULD COMPILE and RUN
Dictionary Exposure of locally defined type classes.
-}

module DictExpose04 where

class Train a where
  choo_choo :: a -> String
  transform :: Transport a => a -> a
  extra_transform :: Transport__Dict a -> a -> a

class Transport a where
  transformTrain :: a -> Train__Dict a -> Train__Dict a
  printTrain :: a -> Train__Dict a -> String

instance Train Bool where
    choo_choo t = "Choooo Chooo!"
    transform x = x
    extra_transform tdict x = x

instance Transport Char where
    transformTrain _ tdict = tdict
    printTrain x tdict = "De IC trein naar Oostende"
