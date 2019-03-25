module Learn where

sayHi :: String -> IO ()
sayHi x = putStrLn ( "Hello " ++ x ++ "!" )


by3 :: Num a => a -> a
by3 = (* 3)

circleArea :: Fractional a => a -> a
circleArea r = 3.14159 * r^2

x = 12 * y
y = 10


-- let .. in versus where versus lambda

foo x = 
    let y = x * 2
        z = x ^ 2
    in y + z

foo' x = y + z
    where y = x * 2
          z = x ^ 2

foo'' x =
    (\y z -> y + z) (x * 2) (x ^ 2)

printInc x =
    let plusTwo = x + 2
    in print plusTwo

printInc' x =
    (\plusTwo -> print plusTwo) (x + 2)


froo x = y
    where y:xs = x

froo' x =
    (\(y : xs) -> y) x 
