module String3 where

greet :: String
greet = "Hello " ++ "world"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()

main = do
    putStrLn greet
    putStrLn secondGreet
    where secondGreet = concat [hello, " ", world]

