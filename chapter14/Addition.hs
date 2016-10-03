module Addition where

import Test.Hspec
import Test.QuickCheck


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)


multBy :: Integral a => a -> a -> a
multBy _ 0 = 0
multBy n m
    | m < 0 = negate $ multBy (-m) n
    | n >= m = n + multBy n (m - 1)
    | otherwise = multBy m n


main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
    describe "Divided By" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
    describe "Multiply By" $ do
        it "0 multiplied by anything is 0" $ do
            multBy 0 3 `shouldBe` 0
        it "anything multiplied by 0 is 0" $ do
            multBy 5 0 `shouldBe` 0
        it "15 multiplied by 3 is 45" $ do
            multBy 15 3 `shouldBe` 45
        it "5 multiplied by -3 is -15" $ do
            multBy 5 (-3) `shouldBe` (-15)
        it "-5 multiplied by 3 is -15" $ do
            multBy (-5) 3 `shouldBe` (-15)
        it "-5 multiplied by -3 is 15" $ do
            multBy (-5) (-3) `shouldBe` 15
    describe "QuickCheck test" $ do
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
        it "0 multiplied by anything is always 0" $ do
            property $ \x -> (x::Int) `multBy` 0 == 0
