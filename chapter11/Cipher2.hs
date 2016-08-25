module Cipher where
import Data.Char (ord, chr)


shift :: Int -> Char -> Char
shift n c = chr (rollOrd n c)
    where
        rollOrd n c = mod ((ord c) + n - (ord 'A')) 26 + ord 'A'

caesar :: Int -> String -> String
caesar n xs = map (shift n) xs

uncaesar :: Int -> String -> String
uncaesar n = caesar (-n)

vigenere :: String -> String -> String
vigenere key xs =
    [ shift n c
    | (n, c) <- zip rotatingKey xs
    ]
    where
        offsetFromA x = ord x - ord 'A'
        rotatingKey = map offsetFromA (concat (repeat key))
-- concat (repeat "ALLY")
