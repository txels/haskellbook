module AsPatterns where
import Data.Char (chr, ord, toUpper)


doubleHead :: [a] -> [a]
doubleHead [] = []
doubleHead xs @ (x:_) = x : xs


-- return True if (and only if) all the values in the first list
-- appear in the second list, though they need not be contiguous
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf (x:xs) [] = False
isSubsequenceOf xs@(x:rxs) (y:ys)
    | x == y = isSubsequenceOf rxs ys
    | otherwise = isSubsequenceOf xs ys


capitalizeWords:: String -> [(String, String)]
capitalizeWords s = [wordPlusCap x | x <- words s]
    where
        wordPlusCap :: String -> (String, String)
        wordPlusCap xs@(x:rxs) = (xs, ((toUpper x):rxs))


testCapitalizeWords :: IO ()
testCapitalizeWords = 
    if capitalizeWords "hello world" == [("hello", "Hello"), ("world", "World")]
    then putStrLn "OK"
    else putStrLn "FAIL"
