module Reverse where


rvrs s = concat [end, " ",  middle, " ", start]
    where end = drop 9 s
          middle = take 2 (drop 6 s)
          start = take 5 s

main :: IO ()
main = putStrLn $ rvrs "Curry is awesome"
