{-
Code sample to understand how records are treated during compilation.
-}

-- Person {Name1 Name2 Age}
data Person = Person { name1 :: String, name2 :: String, age :: Int}

main = do
   putStrLn $ show $ age $ Person "John" "Doe" 24

{- GHC DUMP

===  Typechecker ===

$sel:name1:Person_aE4 Person {name1 = $sel:name1:Person_B1}
  = $sel:name1:Person_B1
$sel:name2:Person_aE5 Person {name2 = $sel:name2:Person_B1}
  = $sel:name2:Person_B1
$sel:age:Person_aE6 Person {age = $sel:age:Person_B1}
  = $sel:age:Person_B1

=== Type Signatures ===

TYPE CONSTRUCTORS
  data type Person{0} :: *
DATA CONSTRUCTORS
  Person :: String -> String -> Int -> Person
-}
