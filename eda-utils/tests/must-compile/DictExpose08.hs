{-  TEST - SHOULD COMPILE
Dictionary Exposure of locally defined type classes.
-}

import Data.Char(toLower)

class Equ a where
  areEq :: a -> a -> Bool
  noEq :: a -> a -> Bool

strCSDict :: Equ__Dict String
strCSDict = Equ__Dict__Con (\s1 s2 -> s1 == s2) (\s1 s2 -> s1 /= s2)

strCIDict :: Equ__Dict String
strCIDict = Equ__Dict__Con (\s1 s2 -> (map toLower s1) == (map toLower s2)) (\s1 s2 -> (map toLower s1) /= (map toLower s2))

f :: Equ__Dict String -> String -> String -> Bool
f (Equ__Dict__Con fe _) s1 s2 = fe s1 s2

y :: Equ__Dict String -> String -> String -> Bool
y (Equ__Dict__Con _ fne) s1 s2 = fne s1 s2

main = do
  let s1 = "KUL"
  let s2 = "Kul"
  putStrLn $ ("[Case Sensitive] " ++ s1 ++ " == " ++ s2 ++ " ?   " ++ show (f strCSDict s1 s2))
  putStrLn $ ("[Case Sensitive] " ++ s1 ++ " /= " ++ s2 ++ " ?   " ++ show (y strCSDict s1 s2))
  putStrLn $ ("[Case Insensitive] " ++ s1 ++ " = " ++ s2 ++ " ?   " ++ show (f strCIDict s1 s2))
  putStrLn $ ("[Case Insensitive] " ++ s1 ++ " /= " ++ s2 ++ " ?   " ++ show (y strCIDict s1 s2))
