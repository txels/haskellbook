module Guards where


myAbs :: Integer -> Integer
myAbs x
    | x > 0   = x
    | otherwise = (-x)
    -- otherwise is equivalent to True


bloodNa :: Integer -> String
bloodNa x
    | x < 135 = "Low"
    | x > 145 = "High"
    | otherwise = "Just right"
    

rightTriangle :: (Eq a, Num a) => a -> a -> a -> Bool
rightTriangle x y z
    | x^2 + y^2 == z^2 = True
    | otherwise        = False


dogYrs :: (Num a, Ord a) => a -> a
dogYrs x
    | x<=0      = 0
    | x<=1      = x * 15
    | x<=2      = x * 12
    | x<=4      = x * 8
    | otherwise = x * 6
