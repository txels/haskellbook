module Patterns where


newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Not registered"
printUser (RegisteredUser (Username name)
                          (AccountNumber number))
    = putStrLn $ "Name: " ++ name ++ " #" ++ (show number)


data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
    Peng WherePenguinsLive
    deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

whereDoesItLive :: Penguin -> WherePenguinsLive
whereDoesItLive (Peng place) = place

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos


galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False


f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))


g :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
g (a, _, c) (d, _, f) = ((a, d), (c, f))



h x = if x + 1 == 1 then "AWESOME" else "wut"
h' x = case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

palindrome xs = case reverse xs == xs of
    True  -> "yes"
    False -> "no"

maxim x y = case x > y of
    True  -> x
    False -> y

nums x = case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
