module Task5_1 where

import Todo(todo)

data DList a = DNil
             | DCons {
                left :: (DList a),
                current :: a,
                right :: (DList a)
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) =
    let rec = DCons left h (list2dlist' rec t)
    in rec

index :: DList a -> Int -> a
index _ n | n < 0 = error "Index out of range"
index DNil _ = error "DList is empty"
index (DCons _ val _) 0 = val
index (DCons _ val t) n = index t (n - 1)

insertAt :: DList a -> Int -> a -> DList a
insertAt _ index _ | index < 0 = error "Index out of range"
insertAt DNil 0 value = DCons DNil value DNil
insertAt DNil _ _ = error "Index out of range"
insertAt (DCons b val t) 0 value = insertAt'
    where
        insertAt' = DCons b value (DCons insertAt' val t)
insertAt (DCons b val t) index value = DCons b val $ insertAt t (index - 1) value

removeAt :: DList a -> Int -> DList a
removeAt _ index | index < 0 = error "Index out of range"
removeAt (DCons b val t@(DCons _ val2 t2)) index
    | index == 0 = DCons b val2 t2
    | otherwise = DCons b val $ removeAt t (index - 1)
removeAt list@(DCons _ _ DNil) index
    | index == 0 = DNil
    | otherwise = list
