module Data.Collection where

  import Data.Array ((:))
  import Data.Tuple

  data SeqView s a = SeqNull
                   | SeqCons a (s a)

  instance showSeqView :: (Show a, Show (s a)) => Show (SeqView s a) where
    show SeqNull = "SeqNull"
    show (SeqCons x xs) = "SeqCons(" ++ show x ++ ", " ++ show xs ++ ")"

  -- | An unordered collection of things.
  --   This class seems to have too many methods.
  --   It might be best to split it up.
  class Collection c a where
    size :: c a -> Number
    cons :: a -> c a -> c a

    filter :: (a -> Boolean) -> c a -> c a
    partition :: (a -> Boolean) -> c a -> Tuple (c a) (c a)

    -- Not-Quite-Foldable, since order is unimportant.
    -- At least, not the standard Foldable.
    fold :: forall b. (a -> b -> b) -> b -> c a -> b

    -- Seems like a Monoid
    unit :: c a
    union :: c a -> c a -> c a

  -- | Collection with some sense of order.
  class (Collection s a) <= Sequence s a where
    snoc :: s a -> a -> s a
    first :: s a -> SeqView s a
    last :: s a -> SeqView s a
    foldl :: forall b. (a -> b -> a) -> a -> s b -> a
    reverse :: s a -> s a

  instance collectionArray :: Collection [] a where
    size [] = 0
    size (_:xs) = 1 + size xs

    cons = (:)

    filter = Data.Array.filter

    partition p xs = Tuple (Data.Array.filter p xs) (Data.Array.filter (not <<< p) xs)

    fold _ z [] = z
    fold f z (x:xs) = x `f` (fold f z xs)

    unit = []

    union = (++)

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
