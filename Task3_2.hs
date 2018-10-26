module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons t h) = h:(rlistToList t)

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList (h:t) = RCons (listToRList t) h

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Eq a) => Eq (ReverseList a)
    where
        (==) RNil RNil = True
        (==) _ RNil = False
        (==) RNil _ = False
        (==) (RCons t1 h1) (RCons t2 h2) = t1 == t2 && h1 == h2

instance (Ord a) => Ord (ReverseList a)
    where
        (<=) RNil _ = True
        (<=) _ RNil = False
        (<=) (RCons t1 h1) (RCons t2 h2) = h1 <= h2 || t1 <= t2

instance (Show a) => Show (ReverseList a) where
    show l = show (rlistToList l)

instance Monoid (ReverseList a)
    where
        mempty = RNil
        mappend RNil l = l
        mappend l RNil = l
        mappend l (RCons t h) = RCons (mappend l t) h

instance Functor ReverseList
    where
        fmap f RNil = RNil
        fmap f (RCons t h) = RCons (fmap f t) (f h)
