module Cipher where
import Data.Char (ord, chr)


caesar :: Int -> String -> String
caesar n xs = map (shift n) xs
    where
        rollOrd n c = mod ((ord c) + n - (ord 'a')) 26 + ord 'a'
        shift n c = chr (rollOrd n c)

uncaesar :: Int -> String -> String
uncaesar n = caesar (-n)
