{-
Code sample to understand how records are treated during compilation.
-}

-- Person {Name1 Name2 Age a} - where a is a type parameter
data Person a = Person String String a

getAge :: Person Int -> Int
getAge (Person name1 name2 age) = age

main = do
   putStrLn $ show $ getAge $ Person "John" "Doe" 77

{- GHC DUMP

== TYPE SIGNATURES ==

TYPE CONSTRUCTORS
  data type Person{1} :: * -> *
DATA CONSTRUCTORS
  Person :: forall a. String -> String -> a -> Person a
-}
