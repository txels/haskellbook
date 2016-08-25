module Filtering where


mul3 :: Integral a => a -> Bool
mul3 = \x -> (rem x 3) == 0

filmul3 :: Integral a => [a] -> [a]
filmul3 xs = filter mul3 xs

nummul3 :: Integral a => [a] -> Int
nummul3 = length . filmul3

articles = ["the", "a", "an"]
noarticles :: String -> [String]
noarticles s = filter (\x -> not $ elem x articles) l
    where l = words s

