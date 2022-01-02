module GADTExample where

-- Random code to understand GADTs
-- Must compile with -XGADTs flag

data Car where
  CarConstructor1 :: String -> Car            -- Constructor
  CarConstructor2 :: String -> String -> Car  -- Constructor
  RandomCar       :: String -> Car            -- Constructor
  deriving (Show)

myCar :: Car
myCar = CarConstructor1 "BMW"

myComplexCar :: Car
myComplexCar = CarConstructor2 "Mercedes" "A Class"

theCarName :: Car -> String
theCarName (CarConstructor1 name) = name
theCarName (CarConstructor2 name model) = name
