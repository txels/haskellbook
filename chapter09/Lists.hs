module Lists where
import Data.Char (toUpper)
import Data.List (intersperse)


-- all of the below are the same:
l1 = (1 : 2 : 3 : 4 : 5 : [])
l2 = [1, 2, 3, 4, 5]
l3 = [1..5]
l4 = enumFromTo 1 5
l5 = [1,2..5]
l6 = enumFromThenTo 1 2 5


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
    | x > y = []
    | x == y = [x]
    | otherwise = [x] ++ eftOrd (succ x) y

eftOrdAll = eftOrd (minBound::Ordering) (maxBound::Ordering)


eftGen :: (Enum a, Ord a) => a -> a -> [a]
eftGen x y
    | x > y = []
    | x == y = [x]
    | otherwise = [x] ++ eftGen (succ x) y


splitOn :: Char -> String -> [String]
splitOn c "" = []
splitOn c xs = takeWhile nomatch xs 
             : splitOn c (safeTail (dropWhile nomatch xs))
    where nomatch = (/= c)
          safeTail :: [a] -> [a]
          safeTail [] = []
          safeTail xs = tail xs

myWords = splitOn ' ' -- standard lib: words
myLines = splitOn '\n'

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen


acronym :: String -> String
acronym xs = [x | x <- xs, elem x ['A'..'Z']]

vowels :: String -> String
vowels xs = [x | x <- xs, elem x "aeiou"]


joinWith :: String -> [String] -> String

joinWith sep xs = foldr (++) "" $ intersperse sep xs
joinWords = joinWith " "  -- standard lib: unwords


myZip :: [a] -> [b] -> [(a, b)] 
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a, b)] 
myZip' = myZipWith (,)

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

capitalizeAll :: String -> String
-- capitalizeAll = map toUpper
capitalizeAll "" = ""
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = x == y || myElem x ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ (squishMap f xs)

squish' :: [[a]] -> [a]
squish' = squishMap id


myCompareBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myCompareBy o f (x:[]) = x
myCompareBy o f (x:xs) = if f x rest == o then x else rest
    where rest = myCompareBy o f xs


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs) = if f x rest == GT then x else rest
    where rest = myMaximumBy f xs


myMaximumBy' = myCompareBy GT
myMinimumBy' = myCompareBy LT


myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy' compare

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy' compare
