module Task6 where

import Todo(todo)

data LinkedTree a = EmptyTree
    | Node a (LinkedTree a) (LinkedTree a) (LinkedTree a) deriving (Eq, Show)

find :: (Ord a) => LinkedTree a -> a -> Bool
find EmptyTree _ = False
find (Node value left right _) a
    | value == a = True
    | value > a = find left a
    | value < a = find right a

insert :: (Ord a) => LinkedTree a -> a -> LinkedTree a
insert EmptyTree a = Node a EmptyTree EmptyTree EmptyTree
insert (Node value EmptyTree right parent) a
    | a < value = newParent
        where newParent = Node value newNode right parent
              newNode = Node a EmptyTree EmptyTree newParent
insert (Node value left EmptyTree parent) a
    | a > value = newParent
        where newParent = Node value left newNode parent
              newNode = Node a EmptyTree EmptyTree newParent
insert x@(Node value left right parent) a
    | a < value = Node value (insert left a) right parent
    | a > value = Node value left (insert right a) parent
    | otherwise = x

remove :: (Ord a) => LinkedTree a -> a -> LinkedTree a
remove EmptyTree _ = EmptyTree
remove (Node value left right parent) a
    | a < value = Node value (remove left a) right parent
    | a > value = Node value left (remove right a) parent
    | otherwise = join left right
        where join x EmptyTree = x
              join x (Node value left right parent) = Node value (join left x) right parent
