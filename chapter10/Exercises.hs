module Exercises where


-- Recap

stops  = "pbtdkg"
vowels = "aeiou"

svs = [[x,y,z] | x <- stops, y <- vowels, z <- stops]
svsp = [[x,y,z] | x <- "p", y <- vowels, z <- stops]
svsp' = filter (\x -> head x == 'p') svs

nouns = ["Mom", "Dad", "Son", "dog"]
verbs = ["eats", "likes", "hugs"]

nvn = [x ++ " " ++ y ++ " " ++ z | x <- nouns, y <- verbs, z <- nouns]


seekritFunc :: Fractional a => String -> a
seekritFunc x =
    (fromIntegral (sum (map length (words x))))
    / (fromIntegral (length (words x)))
-- average word length, rounded to integer.


-- Folding

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> y || f x) False
-- myAny' f xs = foldr (||) False $ map f xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x : y else y) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> f x ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\y x -> if f y x == GT then y else x) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\y x -> if f y x == LT then y else x) x xs

