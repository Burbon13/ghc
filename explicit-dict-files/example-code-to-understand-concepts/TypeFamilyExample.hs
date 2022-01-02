module TypeFamilyExample where

{-
https://wiki.haskell.org/GHC/Type_families

Random code to understand Type Families.
Must compile with -XTypeFamilies flag

indexed type families = type families

They are the data type analogue of type classes: families are used
to define overloaded data in the same way that classes are used to
define overloaded functions.


-}

-- Declare a list-like data family
data family XList a

-- Declare a list-like instance for Char
data instance XList Char =
      XCons !Char !(XList Char)
    | XNil

-- Declare a number-like instance for ()
data instance XList () = XListUnit !Int




