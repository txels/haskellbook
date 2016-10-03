module Arbitrariety where
import Test.QuickCheck

gimmeInts :: IO ()
gimmeInts = sample (arbitrary :: Gen Int)

gimmeTuples :: IO ()
gimmeTuples = sample (arbitrary :: Gen (Int, String))

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen String
oneThroughThree = elements ["one", "two", "three"]
