module Exercises where
import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromJust)


-- replacing "the" with "a"

notThe :: String -> Maybe String
notThe x = if x == "the" then Nothing else Just x


replaceThe :: String -> String
replaceThe "" = ""
replaceThe s = case notThe x of
    Nothing -> "a" ++ " " ++ replaceThe (unwords xs)
    Just y -> y ++ " " ++ replaceThe (unwords xs)
    where (x:xs) = words s


replaceThe' :: String -> String
replaceThe' "" = ""
replaceThe' x = unwords (map replaceTheWord (words x))
    where replaceTheWord x = if x == "the" then "a" else x


-- vowel counting

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel x = find (==x) vowels /= Nothing

beginsWithVowel :: String -> Bool
beginsWithVowel [] = False
beginsWithVowel (x:xs) = (isVowel x)

countTheBeforeVowels :: String -> Integer
countTheBeforeVowels x = countInList (words x)
    where
        countInList :: [String] -> Integer
        countInList [] = 0
        countInList (x:[]) = 0
        countInList (x:xs@(x2:_)) = inc + countInList xs
            where
                -- there is a standard function to replace if:
                inc = bool 0 1 (x == "the" && beginsWithVowel x2)
                -- inc = if x == "the" && beginsWithVowel x2 then 1 else 0


countVowels :: String -> Integer
countVowels [] = 0
countVowels (x:xs) = countVowels xs + (if isVowel x then 1 else 0)

countVowelsAsFold :: String -> Integer
countVowelsAsFold xs = foldr (\x y -> (if isVowel x then 1 else 0) + y) 0 xs


newtype Word' = Word' String
    deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord x
    | vowels > consonants = Nothing
    | otherwise = Just (Word' x)
    where
        vowels = countVowels x
        consonants = fromIntegral (length x) - vowels

-- Natural numbers

data Nat = Zero
         | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x < 0 = Nothing
    | x == 0 = Just Zero
    | otherwise = Just (Succ (fromJust (integerToNat (x - 1))))


-- Library for Maybe ---------------------------------------------

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- catamorphism
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee nothingVal _ Nothing = nothingVal
mayybee _ f (Just x) = f x

-- fallback value
fromMaybe :: a -> Maybe a -> a
fromMaybe def x = mayybee def id x

-- list / maybe conversions
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- drop nothings
catMaybes :: [Maybe a] -> [a]
catMaybes xs = map fromJust $ filter isJust xs
-- catMaybes [Just 1, Nothing, Just 2] == [1,2]

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = case hasNothings of
    True -> Nothing
    False -> Just $ map fromJust xs
    where hasNothings = any isNothing xs


-- Library for Either --------------------------------------------

-- lefts, version one
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

getLeft :: Either a b -> a
getLeft (Left x) = x

lefts' :: [Either a b] -> [a]
lefts' xs = map getLeft $ filter isLeft xs

-- version two, using foldr
lefts'' :: [Either a b] -> [a]
lefts'' xs = foldr addIfLeft [] xs
    where
        addIfLeft (Left x) l = x:l
        addIfLeft _ l = l

rights' :: [Either a b] -> [b]
rights' xs = foldr addIfRight [] xs
    where
        addIfRight (Right x) l = x:l
        addIfRight _ l = l

-- partition
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = foldr addToSection ([],[]) xs
    where
        addToSection (Left x) (lefts, rights) = (x:lefts, rights) 
        addToSection (Right x) (lefts, rights) = (lefts, x:rights) 

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ _ = Nothing

-- catamorphism for eithers
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl fr x = case x of
    Left x' -> fl x'
    Right x' -> fr x'

-- eitherMaybe' using either'
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f) 
