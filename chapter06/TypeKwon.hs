module TypeKwon where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f (x) == y


arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n x = (f x) + fromInteger n
