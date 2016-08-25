module Polymorphic where


data Product a b =
    a :&: b
    deriving Show


data List a = Nil | Cons a (List a)
    deriving Show
