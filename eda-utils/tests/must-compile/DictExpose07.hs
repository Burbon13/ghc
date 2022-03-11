{-  TEST - SHOULD COMPILE
Dictionary Exposure of locally defined type classes.
-}

class Train a where
  chooChoo :: a -> String

singerTrain :: Train__Dict Int
singerTrain = Train__Dict__Con (\x -> "Miley Cyrus")

childTrain :: Train__Dict String
childTrain = Train__Dict__Con (\x -> x ++ " weee weeee, crying")

wonder :: Train__Dict String -> String
wonder (Train__Dict__Con f) = f "Razvan"

main = putStrLn $ wonder childTrain
