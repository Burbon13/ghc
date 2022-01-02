{-
Code sample to understand how records are treated during compilation.
-}

data MyData a = MyDataCon (a -> a -> Bool) (a -> a -> Bool)

execFirstMethod :: MyData a -> a -> a -> Bool
execFirstMethod (MyDataCon f1 f2) p1 p2 = f1 p1 p2

myLocalData = MyDataCon (\x y -> True) (\x y -> False)

main = do
  putStr $ show $ execFirstMethod myLocalData 3 4

{- GHC DUMP

== TYPE SIGNATURES ==

TYPE CONSTRUCTORS
  data type MyData{1} :: * -> *
DATA CONSTRUCTORS
  MyDataCon :: forall a.
               (a -> a -> Bool) -> (a -> a -> Bool) -> MyData a
-}
