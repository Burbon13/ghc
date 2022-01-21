{-  TEST - SHOULD COMPILE and RUN
Dictionary Exposure of locally defined type classes.
-}

class Train a where
  choo_choo :: a -> String

f1 :: Train__Dict b -> Int -> Int
f1 x y = y

f2 :: Train__Dict b -> Train__Dict b
f2 x = x

f3 :: (Train__Dict b -> Int) -> Train__Dict b -> Int
f3 f x = f x

f4 :: (Train__Dict b -> Train__Dict b) -> Train__Dict b -> Train__Dict b
f4 f t = f t

main = putStrLn "Trainsss!"
