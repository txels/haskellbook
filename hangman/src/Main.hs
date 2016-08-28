module Main where

import Control.Monad (forever)
import Data.Char (isUpper, toLower)
import Data.Maybe (catMaybes, isJust)
import Data.List (intersperse, nub, sort)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


newtype WordList = WordList [String]
    deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxWrongGuesses :: Int
maxWrongGuesses = 10

gameLength :: String -> Bool
gameLength x = len > minWordLength && len < maxWordLength
    where len = length x

isCommonNoun :: String -> Bool
isCommonNoun (x:xs) = not (isUpper x)

acceptableWord :: String -> Bool
acceptableWord x = isCommonNoun x && gameLength x

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter acceptableWord aw)

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- The word we are guessin,
-- the characters filled in so far,
-- the letters we guessed so far
data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " - Guessed so far: " ++ sort guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle x = Puzzle x (replicate (length x) Nothing) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c : s)
    where
        zipper guessed wordChar guessedChar =
            if guessed == wordChar
            then Just guessed
            else guessedChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed this"
            return puzzle
        (True, _) -> do
            putStrLn "Good guess"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "Nopes, try again..."
            return (fillInCharacter puzzle guess)


incorrectGuesses :: Puzzle -> Int
incorrectGuesses (Puzzle _ _ []) = 0
incorrectGuesses (Puzzle _ filledIn guessed) =
    let guessedRight = nub (catMaybes filledIn)
        guessedWrong = filter (\x -> not (elem x guessedRight)) guessed
    in length guessedWrong


-- TODO: count only failed guesses
gameOver :: Puzzle -> IO ()
gameOver puzzle @ (Puzzle wordToGuess _ guessed) =
    if incorrectGuesses puzzle >= maxWrongGuesses
    then
        do putStrLn "Lost it!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else
        return ()

gameWin :: Puzzle -> IO ()
gameWin puzzle @ (Puzzle wordToGuess filledInSoFar guessed) =
    if all isJust filledInSoFar
    then
        do putStrLn $
            "You won using " ++
            show (length guessed) ++
            " guesses, " ++
            show (incorrectGuesses puzzle) ++
            " of which were wrong."
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else
        return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "State: " ++ show puzzle
    putStr "Make a guess: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "You have to use a single character!"


main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle word
    runGame puzzle

