module Eqs where

data Trivial =
    Trivial

instance Eq Trivial where
    Trivial == Trivial = True


data DayOfWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _   _   = False

-- Ord making Friday the greatest day of the week
instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _   = GT
    compare _   Fri = LT
    compare _   _   = EQ


data Date =
    Date DayOfWeek Int

instance Eq Date where
    (==) (Date weekday num) (Date weekday' num') =
        weekday == weekday' && num == num'


data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y


data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = (x, y) == (x', y')


data StringOrInt =
    AnInt Integer | AString String

instance Eq StringOrInt where
    (==) (AString x) (AString y) = x == y
    (==) (AnInt x) (AnInt y) = x == y
    (==) _ _ = False


data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = (x, y) == (x', y')


data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y) = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _ = False
