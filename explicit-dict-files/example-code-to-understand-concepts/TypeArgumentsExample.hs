{-# LANGUAGE TypeApplications #-}

module TypeArgumentsExample where

answer_read = show (read @Int "3")
ambigous_read = show (read "3")

data IString = IString String

instance Eq IString where
  (==) (IString a) (IString b) = True

fancy :: Eq a => a -> a -> String
fancy x y = if x == y then "Egale" else "Diferite"


