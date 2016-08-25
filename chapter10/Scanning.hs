module Scanning where


-- fibonacci using scan:
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsNth n = fibs !! n

fac :: [Integer]
fac = scanl (*) 1 [1..]
factorial n = fac !! n
