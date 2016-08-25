module WordNumber where
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"

digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise =
        digits (d) ++ [m]
        where (d, m) = divMod n 10
    

wordNumber :: Int -> String
wordNumber n = foldl (++) "" list
    where list = intersperse "-" (map digitToWord (digits n))

wordNumber' :: Int -> String
wordNumber' n = concat list
    where list = intersperse "-" (map digitToWord (digits n))
