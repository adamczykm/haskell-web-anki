module NonEmpty where

infixr 5 :|
data NonEmpty a = a :| [a]
  deriving (Eq, Ord, Show, Read)

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| map f xs

instance Foldable NonEmpty where
  foldMap f (x :| xs) = foldMap f (x : xs)

nonEmptyList :: NonEmpty a -> [a]
nonEmptyList (x:|xs) = x : xs

neHead :: NonEmpty a -> a
neHead (x:|_) = x
