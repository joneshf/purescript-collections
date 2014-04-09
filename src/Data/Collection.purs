module Data.Collection where

  import Data.Array ((:))
  import Data.Monoid
  import Data.Tuple

  data SeqView s a = SeqNull
                   | SeqCons a (s a)

  instance showSeqView :: (Show a, Show (s a)) => Show (SeqView s a) where
    show SeqNull = "SeqNull"
    show (SeqCons x xs) = "SeqCons(" ++ show x ++ ", " ++ show xs ++ ")"

  -- | This differs from the standard Foldable
  --   in that it doesn't attempt to impose an ordering.
  class Foldable f where
    fold :: forall a b. (a -> b -> b) -> b -> f a -> b

  -- | An unordered collection of things.
  class (Foldable c, Monoid (c a)) <= Collection c a where
    -- `size` and `cons` are the minimum required to implement a `Collection`
    size :: c a -> Number
    cons :: a -> c a -> c a

    -- `filter` and `partition` should have default implementations.
    filter :: (a -> Boolean) -> c a -> c a
    -- filter p xs = fold (\x acc -> if p x then cons x acc else acc) unit xs
    partition :: (a -> Boolean) -> c a -> Tuple (c a) (c a)
    -- partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)

  -- | Collection with some sense of order.
  class (Collection s a) <= Sequence s a where
    -- Minimum definition needs `foldl`, and one of `first` or `last`.
    foldl :: forall b. (a -> b -> a) -> a -> s b -> a
    first :: s a -> SeqView s a
    -- first xs = case last (reverse xs) of
    --   SeqCons x xs' -> SeqCons x (reverse xs')
    --   SeqNull -> SeqNull
    snoc :: s a -> a -> s a
    -- snoc xs x = reverse (cons x (reverse xs))
    last :: s a -> SeqView s a
    -- last xs = case first (reverse xs) of
    --   SeqCons x xs' -> SeqCons x (reverse xs')
    --   SeqNull -> SeqNull
    reverse :: s a -> s a
    -- reverse xs = foldl (flip cons) mempty xs

  instance foldableArray :: Foldable [] where
    fold _ z [] = z
    fold f z (x:xs) = x `f` (fold f z xs)

  instance collectionArray :: Collection [] a where
    size [] = 0
    size (_:xs) = 1 + size xs

    cons = (:)

    filter p xs = fold (\x acc -> if p x then cons x acc else acc) mempty xs
    partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)

  instance sequenceArray :: Sequence [] a where
    snoc xs x = xs ++ [x]

    first [] = SeqNull
    first (x:xs) = SeqCons x xs

    last [] = SeqNull
    last [x] = SeqCons x []
    last (x:xs) = case last xs of
      SeqCons x' xs' -> SeqCons x' (x:xs')

    foldl _ z [] = z
    foldl f z (x:xs) = foldl f (z `f` x) xs

    reverse = Data.Array.reverse
