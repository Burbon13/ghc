{-
Code sample to understand how records are treated during compilation.
-}

-- Person {Name1 Name2 Age a} - where a is a type parameter
data Person a b = Person String String a b

getAge :: Person Int String -> Int
getAge (Person name1 name2 age _) = age

main = do
   putStrLn $ show $ getAge $ Person "John" "Doe" 77 "N/A"

{- GHC DUMP

== TYPE SIGNATURES ==

TYPE CONSTRUCTORS
  data type Person{2} :: * -> * -> *
DATA CONSTRUCTORS
  Person :: forall a b. String -> String -> a -> b -> Person a b
-}
