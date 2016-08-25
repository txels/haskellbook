module Recursion where


factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)


applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes times n = applyTimes times (+1) n 


-- slow
fibN :: Integral a => a -> a
fibN 0 = 1
fibN 1 = 1
fibN n = (fibN (n - 1)) + (fibN (n - 2))
    
fib :: Integral a => Int -> [a]
fib n = take n (map fibN (enumFrom 0))

-- high performance:
fib' :: Int -> [Integer]
fib' 0 = [1]
fib' 1 = [1, 1]
fib' n = prev ++ [val] where
    prev = fib' (n - 1)
    val = (last prev) + (prev !! ((length prev) - 2))


-- mine
divideInt :: Integer -> Integer -> (Integer, Integer)
divideInt n m = if n < m then (0, n) else
    let (quo, rem) = divideInt (n - m) m
    in (quo + 1, rem)


data DivisionResult = Result Integer | DividedByZero
    deriving Show

-- based on book's solution
divideBy :: Integral a => a -> a -> DivisionResult
divideBy num 0 = DividedByZero
divideBy num den
    | signum num == signum den = go num den 0
    | otherwise = let Result n = go (-num) den 0
                  in Result (-n)
    where go n d count
            | (abs n < abs d) = Result count
            | otherwise = go (n - d) d (count + 1)


add1To :: (Eq a, Num a) => a -> a
add1To n = go 0 n where
    go acc 0 = acc
    go acc count = go (acc + count) (count - 1)

add1To' :: (Eq a, Num a) => a -> a
add1To' 1 = 1
add1To' n = n + add1To' (n - 1)


multBy :: Integral a => a -> a -> a
multBy n 0 = 0
multBy n m
    | n >= m = n + multBy n (m - 1)
    | otherwise = multBy m n


mac91 :: Integral a => a -> a
mac91 n
    | n > 100 = n - 10
    | otherwise = mac91 (mac91 (n + 11))

    
