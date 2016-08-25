module Exercises where
import Data.Char (toUpper)


sent = "this is here.   this is now."

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord xs@(x:rxs) = (toUpper x):rxs

-- without using capitalizeWord, as a fold
capitalizeParagraph :: String -> String
capitalizeParagraph x = zipWith toUpperIfTrue (shouldCap x) x
    where
        capNext :: Char -> Bool -> Bool
        capNext ' ' x = x
        capNext '.' _ = True
        capNext _ _ = False
        toUpperIfTrue :: Bool -> Char -> Char
        toUpperIfTrue b x = if b then toUpper x else x
        shouldCap :: [Char] -> [Bool]
        shouldCap x = scanl (flip capNext) True x
-- doesn't work as foldr...

-- using capitalizeWord
capitalizeParagraph' :: String -> String
capitalizeParagraph' x = undefined
