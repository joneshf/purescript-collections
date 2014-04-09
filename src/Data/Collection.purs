module Data.Collection where

  import Data.Array ((:))
  import Data.Tuple

  -- | An unordered collection of things.
  class Collection c a where
    size :: c a -> Number
    cons :: a -> c a -> c a

    filter :: (a -> Boolean) -> c a -> c a
    partition :: (a -> Boolean) -> c a -> Tuple (c a) (c a)

    -- Not-Quite-Foldable, since order is unimportant.
    fold :: forall b. (a -> b -> b) -> b -> c a -> b

    -- Seems like a Monoid
    unit :: c a
    union :: c a -> c a -> c a

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
