module Unfold where


iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f acc = case res of
    Nothing -> []
    (Just (x, y)) -> x : myUnfoldr f y
    where res = f acc

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x


-- Tree unfolding

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f acc = case res of
    Nothing -> Leaf
    Just (lacc, x, racc) -> Node (unfold f lacc) x (unfold f racc)
    where res = f acc

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold fun n
    where
        fun b = if b == 0
                then Nothing
                else Just (b-1, n-b, b-1)
