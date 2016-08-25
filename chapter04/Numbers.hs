module Numbers where

import GHC.Int

rangeInt = ( minBound :: Int, maxBound :: Int )
rangeInt8 = ( minBound :: Int8, maxBound :: Int8 )
rangeInt16 = ( minBound :: Int16, maxBound :: Int16 )
rangeInt32 = ( minBound :: Int32, maxBound :: Int32 )

rangeChar = ( minBound :: Char, maxBound :: Char )

-- /= is for not equal
different :: Eq a => a -> a -> Bool
different = (/=)

tellMeDiff x y = putStrLn $
    if different x y
    then "Yes"
    else "No"

absVal :: (Ord a, Num a) => a -> a
absVal x = if x >= 0 then x else negate x 
