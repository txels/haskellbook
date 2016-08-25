module Normal where

-- distributive property (prod distributes over sum)

-- type synonym
type AuthorName = String

-- not normal form:
-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show
-- data BookType = FictionBook Fiction
--              | NonfictionBook Nonfiction
--              deriving Show
-- data Author = Author (AuthorName, BookType)

-- normal form: expressed as sum of products
-- rather than product of sums
data Author = Fiction AuthorName      -- ~ Fiction * AuthorName
            | Nonfiction AuthorName   -- ~ Nonfiction * AuthorName
            deriving (Eq, Show)


data Expr =
    Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr
  deriving Show


type Gardener = String
-- non-normal:
-- data FlowerType = Gardenia
                -- | Daisy
                -- | Rose
                -- | Lilac
                -- deriving Show
-- data Garden =
  -- Garden Gardener FlowerType
  -- deriving Show


data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving (Eq, Show)
