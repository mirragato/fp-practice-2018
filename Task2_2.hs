module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini [] = ini
foldl f ini (x:xs) = foldl f (f ini x) xs


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = f x (foldr f ini xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
    Just (a, b') -> a : unfoldr f b'
    Nothing -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:).f) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldr (*) 1

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> (maybeToList x) ++ acc) []

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal mat = reverse $ snd $ foldl diagonal' (0, []) mat
    where
        counter _ count = count + 1
        sizeOfRow = foldr counter 0 $ head mat
        diagonal' (size, acc) sizeOfRow
            | length sizeOfRow <= size = (size + 1, acc)
            | otherwise = (size + 1, sizeOfRow !! size: acc)
        snd (_, snd') = snd'

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot predicat lst = foldr f [] lst
    where f x acc = if predicat x then acc else (x: acc)

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x = foldl (\s x' -> if x' == x then True else s) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\x -> if x < to then Just (x, x + step) else Nothing) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append listFirst listSecond = foldr (\x acc -> x: acc) listSecond listFirst

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = reverse . (map reverse) . fst $ foldl group' ([], 0) lst
    where
        group' = (group'' n)
        group'' _ ([], 0) elem = ([[elem]], 1)
        group'' size' ((x : acc), size) elem
            | size == size' = ([elem] : x : acc, 1)
            | otherwise = ((elem : x) : acc, size + 1)
