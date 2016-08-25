module Types where

f :: a -> a -> a
f x y = y
--f x y = x -- the only alternative implementation


g :: a -> b -> b

g x y = y


-- type inference

h :: Num a => a -> a -> a
h x y = x + y + 3

-- type signature will be inferred, and same as h
h' x y = x + y + 3

r :: [a] -> [a]
r x = take 4 x

r' :: [a] -> [a]
r' x = reverse x

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g x = f $ g x

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f x = f x


data Anon = Anonymous | Named String
    deriving (Eq, Show, Ord)
