module Task6 where

import Todo(todo)

data LinkedTree a = EmptyTree
    | Node a (LinkedTree a) (LinkedTree a) (LinkedTree a) deriving (Eq)

instance (Show a) => Show (LinkedTree a) where
    show EmptyTree = "EmptyTree"
    show (Node v left right parent) = 
        concat ["Node ", show v, " (", show left, ")", " (", show right, ")"]

find :: (Ord a) => LinkedTree a -> a -> Bool
find EmptyTree _ = False
find (Node value left right _) a
    | value == a = True
    | value > a = find left a
    | value < a = find right a

parentsShow :: (Show a) => LinkedTree a -> IO ()
parentsShow (Node v left right parent) = do
    print $ show v <> ": " <> show parent
    parentsShow left
    parentsShow right
parentsShow (EmptyTree) = do
    print $ "parent on EmptyTree"

insert :: (Ord a) => LinkedTree a -> a -> LinkedTree a
insert EmptyTree a = Node a EmptyTree EmptyTree EmptyTree
insert (Node value EmptyTree right parent) a
    | a < value = newParent
        where newParent = Node value newNode r (inParentInsert parent a)
              r = changeParentNode right newParent
              newNode = Node a EmptyTree EmptyTree newParent
insert (Node value left EmptyTree parent) a
    | a > value = newParent
        where newParent = Node value l newNode (inParentInsert parent a)
              l = changeParentNode left newParent
              newNode = Node a EmptyTree EmptyTree newParent
insert x@(Node value left right parent) a
    | a < value = let newNode = Node value (insert left a) r (inParentInsert parent a) 
                      r = changeParentNode right newNode
                  in newNode
    | a > value = let newNode = Node value l (insert right a) (inParentInsert parent a)
                      l = changeParentNode left newNode
                  in newNode
    | otherwise = x

inParentInsert EmptyTree _ = EmptyTree
inParentInsert parent a = insert parent a 

changeParentNode node newNode = case node of
            (Node v l r p) -> Node v l r newNode
            emptyTree      -> emptyTree

inParentRemove EmptyTree _ = EmptyTree
inParentRemove parent a = remove parent a

remove :: (Ord a) => LinkedTree a -> a -> LinkedTree a
remove EmptyTree _ = EmptyTree
remove (Node value left right parent) a
    | a < value = let newNode = Node value (remove left a) r (inParentRemove parent a)
                      r = changeParentNode right newNode
                  in newNode
    | a > value = let newNode = Node value l (remove right a) (inParentRemove parent a)
                      l = changeParentNode left newNode
                  in newNode
    | otherwise = case right of
        (Node v l r p) -> let newNode = join left (Node v l newR (inParentRemove parent a))
                              newR = changeParentNode r newNode 
                          in newNode
        emptyTree      -> join left emptyTree
        where  
            join x (Node value EmptyTree right parent) = 
                let newNode = Node value l r parent
                    r = changeParentNode right newNode
                    l = case x of
                        EmptyTree -> EmptyTree
                        (Node lv ll lr lp) -> (Node lv ll lr newNode)  
                in newNode
            join x (Node value left right parent) = 
                let newNode = Node value (join x l) r parent
                    l = changeParentNode left newNode
                    r = changeParentNode right newNode
                in newNode

-- some test trees
tree1 = insert (insert (insert (insert EmptyTree 10) 5) 20) 3
tree2 = insert (insert (insert tree1 8 ) 15) 25
tree3 = insert (insert (insert (insert tree2 1) 4) 6) 9
treeFull = insert (insert (insert (insert tree3 12) 17) 22) 27

treeFullRemove20 = remove treeFull 20

treeFullRemove10 = remove treeFull 10
