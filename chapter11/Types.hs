{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where


class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 40

newtype Goats = Goats Int
    deriving (Eq, Show, TooMany)

newtype NamedInts = NamedInt (Int, String)
    deriving (Eq, Show)

instance TooMany NamedInts where
    tooMany (NamedInt (n, _)) = n > 40

-- without newtype:
-- requires pragma FlexibleInstances
instance TooMany (Int, String) where
    tooMany (n, _) = n > 40

instance TooMany (Int, Int) where
    tooMany (n, m) = n + m > 40

instance (Ord a, Num a, TooMany b) => TooMany (a, b) where
    tooMany (n, m) = n > 40 || tooMany m


type TwoBools = (Bool, Bool)

