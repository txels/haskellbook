module Lists where

first3 :: [a] -> [a]
first3 = take 3

h:t = [1,2,3,4,5]
-- h = 1
-- t = [2,3,4,5]

behead3 :: [a] -> [a]
behead3 = drop 3

-- sample with first, last, and center element
sample :: [a] -> [a]
sample [] = []
sample l = [head l, l !! middle, last l]
    where middle = div (length l) 2

thirdLetter :: String -> Char
thirdLetter s = s !! 2

nthLetter :: Int -> Char
nthLetter n = "Curry is awesome!" !! (n - 1)

awesome = "Curry is awesome"

rvrs = concat [end, " ",  middle, " ", start]
    where end = drop 9 awesome
          middle = take 2 (drop 6 awesome)
          start = take 5 awesome
