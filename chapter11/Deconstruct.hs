module Deconstruct where 

newtype Name  = Name String deriving (Eq, Show)
newtype Acres = Acres Int deriving (Eq, Show)

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving (Eq, Show)

data Farmer = Farmer Name Acres FarmerType
    deriving (Eq, Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False


data FarmerRec =
    FarmerRec { name :: Name
              , acres :: Acres
              , farmerType :: FarmerType
              }
    deriving Show
isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = farmerType farmer == DairyFarmer
