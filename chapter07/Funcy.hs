module Funcy where


tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
    where (x10th, _) = x `divMod` 10
          (_, d) = x10th `divMod` 10

hunsDigit :: Integral a => a -> a
hunsDigit = tensDigit . (flip div 10)

-- equivalent to standard (flip bool)
foldBool :: a -> a -> Bool -> a
foldBool x y b = if b then x else y

foldBool' :: a -> a -> Bool -> a
foldBool' x y True = x
foldBool' x y False = y

foldBool'' :: a -> a -> Bool -> a
foldBool'' x y b
    | b = x
    | otherwise = y

foldBool''' :: a -> a -> Bool -> a
foldBool''' x y b = case b of
    True -> x
    False -> y


g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y) 
