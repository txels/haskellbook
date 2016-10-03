module Cipher3 where
import Data.Char (ord, chr)


shift :: Int -> Char -> Char
shift n c = chr (rollOrd n c)
    where
        rollOrd n c = mod ((ord c) + n - (ord 'A')) 26 + ord 'A'

caesar :: Int -> String -> String
caesar n xs = map (shift n) xs

iocaesar :: Int -> String -> IO String
iocaesar n xs = do
    return $ map (shift n) xs

main :: IO ()
main = do
    putStr "Enter a word: "
    word <- getLine
    result <- iocaesar 7 word
    putStrLn result
