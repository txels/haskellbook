module Vehicles where


data Price = Price Integer
    deriving (Eq, Show)

data Size = Big | Medium | Small
    deriving (Eq, Show)

data Manufacturer = Seat | Honda | Opel
    deriving (Eq, Show)

data Airline = Vueling | EasyFly | TurboWings
    deriving (Eq, Show)

data Vehicle
    = Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)


myCar = Car Seat (Price 14000)
urCar = Car Honda (Price 20000)
clownCar = Car Opel (Price 7000)
doge = Plane EasyFly Medium

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x
