module Folding where


-- use with showFolding foldr|foldl
showFolding foldtype = foldtype (\x y -> concat ["(",x,"+",y,")"]) "0" xs
    where xs = map show [1..5]


-- eval steps for foldl (flip (*)) 1 [1..3]
-- (((1 * 1) * 2) * 3)
--


-- Ways to think about folding
-- foldr: accumulator combines with last elem: f last acc
-- foldl: accumulator combines with first elem: f acc first
