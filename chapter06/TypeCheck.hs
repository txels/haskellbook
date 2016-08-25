module TypeCheck where

-- sort
import Data.List


data Person = Person Bool
    deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x =
    if x == Woot
    then Blah
    else x


data Rocks =
    Rocks String deriving (Eq, Show)
data Yeah =
    Yeah Bool deriving (Eq, Show)
data Papu =
    Papu Rocks Yeah deriving (Eq, Show)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'


mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)
