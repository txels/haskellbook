module StringFlip where

myGreeting :: String
myGreeting = (++) "Hello" " Carles"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()

main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where
        secondGreeting = (++) hello ((++) " " world)
