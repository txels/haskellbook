module Tree where


data Tree a =
      Leaf
    | Node (Tree a) a (Tree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> Tree a -> Tree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node left y right) =
    if x < y
    then
        Node (insert' x left) y right
    else
        Node left y (insert' x right)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) =
    Node (mapTree f left) (f x) (mapTree f right)

-- TESTS

-- test the mapTree function:
testTree' :: Tree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay :: IO ()
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"


preorder :: Tree a -> [a]
preorder Leaf = []
preorder (Node left x right) = x : (preorder left) ++ (preorder right)

postorder :: Tree a -> [a]
postorder Leaf = []
postorder (Node left x right) = (postorder left) ++ (postorder right) ++ [x]

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node left x right) = (inorder left) ++ [x] ++ (inorder right)

testTree :: Tree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- fold using a defined traversal
foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f acc tree = foldr f acc (inorder tree)

-- fold without using a defined traversal
foldTree' :: (a -> b -> b) -> b -> Tree a -> b
foldTree' _ acc Leaf = acc
foldTree' f acc (Node left x right) =
    foldTree' f (f x accleft) right
    where
        accleft = foldTree' f acc left
