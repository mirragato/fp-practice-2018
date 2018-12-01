module Task7 where

import Todo(todo)

data Deque a = Deque [a] [a] Int deriving (Show, Eq)

-- Пустая очередь
empty :: Deque a
empty = Deque [] [] 0

-- Добавление в начало очереди (соответствует enqueue из лекции)
pushFront :: Deque a -> a -> Deque a
pushFront (Deque front back size) x = Deque (x : front) back (size + 1)

-- Удаление из начала очереди
popFront :: Deque a -> (a, Deque a)
popFront (Deque [] [] _) = error "Empty!"
popFront (Deque (x : other) back size) = (x, Deque other back (size - 1))
popFront (Deque [] back size) = popFront $ Deque (reverse $ snd split) (fst split) size
    where
        split = splitAt backSize back
        backSize = quot size 2

-- Добавление в конец очереди
pushBack :: Deque a -> a -> Deque a
pushBack (Deque front back size) x = Deque front (x : back) (size + 1)

-- Удаление из конца очереди (соответствует dequeue из лекции)
popBack :: Deque a -> (a, Deque a)
popBack (Deque [] [] _) = error "Empty!"
popBack (Deque front (x : other) size) = (x, Deque front other (size - 1))
popBack (Deque front [] size) = popBack $ Deque (fst split) (reverse $ snd split) size
    where
        split = splitAt frontSize front
        frontSize = quot size 2

{-
    Метод банкира: Для получения очереди размера N необходимо получить N$, т.к перемещается
    только половина списка, перемещение списка происходит не чаще одного раза в N/2,
    то есть линенйное число операций. Таким образом, амортизированная сложность - константа.

    Метод физика: Потенциал - размер очереди. Добавление в непустую очередь увеличивает
    потенциал на 1. Удаление без перемещения не меняет потенциал.Удаление с перемещением
    уменьшает потенциал на N.Потенциал не отрицателен, потому что перемещаемое количество
    элементов не может быть больше размера очереди, а между перемещениями происходит не менее
    N/2 операций.
-}
