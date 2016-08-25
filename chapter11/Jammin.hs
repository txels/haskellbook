module Jammin where

import Data.List


data Fruit = 
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars =
  Jam { fruit :: Fruit
      , jars :: Int }
  deriving (Eq, Show, Ord)

row1 = [Jam Plum 12, Jam Apple 4]
row2 = [Jam Peach 4, Jam Blackberry 4]
row3 = [Jam Plum 2, Jam Peach 4]
row4 = [Jam Blackberry 3, Jam Blackberry 1, Jam Apple 2]
row5 = [Jam Plum 1, Jam Apple 6]
row6 = [Jam Peach 3, Jam Apple 5]
allJam = [row1, row2, row3, row4, row5, row6]

flatJam = concat allJam

totalJars = sum $ map jars (concat allJam)

compareBySize :: JamJars -> JamJars -> Ordering
compareBySize x y = compare (jars x) (jars y)

compareByKind :: JamJars -> JamJars -> Ordering
compareByKind (Jam x _) (Jam y _) = compare x y

mostRow = last (sortBy compareBySize $ concat allJam)

groupedByKind = groupBy (\x y -> compareByKind x y == EQ) $ sortBy compareByKind flatJam
