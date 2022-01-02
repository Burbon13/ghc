{-
Code sample to understand how records are treated during compilation.
-}

-- Person {Name1 Name2 Age}
data Person = Person String String Int

getAge :: Person -> Int
getAge (Person name1 name2 age) = age

main = do
   putStrLn $ show $ getAge $ Person "John" "Doe" 23

{- GHC DUMP

== TYPE SIGNATURES ==

TYPE CONSTRUCTORS
  data type Person{0} :: *
DATA CONSTRUCTORS
  Person :: String -> String -> Int -> Person
-}
