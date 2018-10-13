module Task1_1 where

import Todo(todo)

data Operation = Plus | Minus | Multiply deriving(Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, operation :: Operation, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)


-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l Plus r
infixl 1 |+|
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l Minus r
infixl 1 |-|
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l Multiply r
infixl 2 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
    let replace x = replaceVar varName replacement x in
        case expression of
            Variable var | var == varName -> replacement
            BinaryTerm lhv operation rhv -> BinaryTerm (replace lhv) operation (replace rhv)
            _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
    BinaryTerm lhv operation rhv -> case (l, operation, r) of
        (IntConstant l, Plus, IntConstant r) -> IntConstant (l + r)
        (IntConstant 0, Plus, r) -> r
        (l, Plus, IntConstant 0) -> l
        (IntConstant l, Minus, IntConstant r) -> IntConstant (l - r)
        (l, Minus, IntConstant 0) -> l
        (IntConstant l, Multiply, IntConstant r) -> IntConstant (l * r)
        (IntConstant 0, Multiply, r) -> IntConstant 0
        (IntConstant 1, Multiply, r) -> r
        (l, Multiply, IntConstant 0) -> IntConstant 0
        (l, Multiply, IntConstant 1) -> l
        _ -> BinaryTerm l operation r
        where
            l = evaluate(lhv)
            r = evaluate(rhv)
    _ -> expression
