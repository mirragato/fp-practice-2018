module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative FourOf where
    pure x = FourOf x x x x
    FourOf a b c d <*> FourOf a1 b1 c1 d1 = FourOf (a a1) (b b1) (c c1) (d d1)

instance Monad FourOf where
    return = pure
    FourOf a b c d >>= x = FourOf (f1 (x a)) (f2 (x b)) (f3 (x c)) (f4 (x d))
        where
            f1 (FourOf x _ _ _) = x
            f2 (FourOf _ x _ _) = x
            f3 (FourOf _ _ x _) = x
            f4 (FourOf _ _ _ x) = x
