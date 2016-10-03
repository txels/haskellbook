import Control.Monad
import Data.Char (toLower)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (line1 == reverse line1) of
        True -> do 
            putStrLn "It's a palindrome!"
            exitSuccess
        False -> putStrLn "Nope!"

letters = ['a'..'z']
isLetter :: Char -> Bool
isLetter c = elem c letters

normalize :: String -> IO String
normalize x = do
    let lower = map toLower x
    return $ filter isLetter lower


palindromeSentence :: IO ()
palindromeSentence = do
    sentence <- getLine
    normalized <- normalize sentence
    case (normalized == reverse normalized) of
        True -> do 
            putStrLn "It's a palindrome!"
            exitSuccess
        False -> putStrLn "Nope!"
    
