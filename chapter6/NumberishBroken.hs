module NumberishBroken where

class NumberishBroken a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer
    defaultNumber :: a


newtype Age = Age Integer
    deriving (Eq, Show)

instance NumberishBroken Age where
    fromNumber n = Age n
    toNumber (Age n) = n
    defaultNumber = Age 65


newtype Year = Year Integer
    deriving (Eq, Show)

instance NumberishBroken Year where
    fromNumber n = Year n
    toNumber (Year n) = n
    defaultNumber = Year 1988

