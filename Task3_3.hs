module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet2 a = PSet2{ contains2 :: (a -> Bool) }
newtype PSet3 a = PSet3{ contains3 :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- сложение множеств
instance Monoid (PSet a) where
  mempty = PSet (\a -> False)
  mappend (PSet f1) (PSet f2) = PSet (\a -> (f1 a) || (f2 a))

instance Semigroup (PSet a) where
    (<>) l r = mappend l r

-- пересечение множеств
instance Monoid (PSet2 a) where
  mempty = PSet2 (\a -> True)
  mappend (PSet2 f1) (PSet2 f2) = PSet2 (\a -> (f1 a) && (f2 a))

instance Semigroup (PSet2 a) where
    (<>) l r = mappend l r

-- симметрическая разность
instance Monoid (PSet3 a) where
  mempty = PSet3 (\a -> False)
  mappend (PSet3 f1) (PSet3 f2) = PSet3 (\a -> ((f1 a) && (not $ f2 a)) || ((not $ f1 a) && (f2 a)))

instance Semigroup (PSet3 a) where
    (<>) l r = mappend l r

-- функтор
-- Результат всегда возвращает False, потому что зная только отображение из множества A в B
-- мы не можем сказать ничего о множестве B
instance Functor PSet where
  fmap f (PSet fa) = PSet (\b -> False)
