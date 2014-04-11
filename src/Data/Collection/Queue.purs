module Data.Collection.Queue where

  import Data.Collection
  import Data.Collection.Foldable
  import Data.Collection.Sequence

  import Data.Maybe
  import Data.Monoid
  import Data.Tuple

  -- This should have O(1) insert and removal.
  data Queue a = EmptyQueue
               | Enqueue a (Queue a)

  instance showQueue :: (Show a) => Show (Queue a) where
    show EmptyQueue = "EmptyQueue"
    show (Enqueue q qs) = "Queue(" ++ show q ++ ", " ++ show qs ++ ")"
  -- Queue

  instance foldableQueue :: Foldable Queue a where
    fold _ z EmptyQueue = z
    fold f z (Enqueue q qs) = q `f` (fold f z qs)

  instance semigroupQueue :: Semigroup (Queue a) where
    (<>) EmptyQueue q = q
    (<>) q EmptyQueue = q
    (<>) (Enqueue q qs) qs' = Enqueue q (qs <> qs')

  instance monoidQueue :: Monoid (Queue a) where
    mempty = EmptyQueue

  instance collectionQueue :: Collection Queue a where
    add q qs = snoc qs q
    singleton q = Enqueue q EmptyQueue

  instance sequenceQueue :: Sequence Queue a where
    snoc EmptyQueue q = Enqueue q EmptyQueue
    snoc (Enqueue q' qs) q = Enqueue q' (snoc qs q)

    foldr f z qs = fold f z qs

    front EmptyQueue = Nothing
    front (Enqueue q qs) = Just (Tuple q qs)

    -- Use default implementations.
    cons q qs = reverse (snoc (reverse qs) q)

    foldl f z qs = foldr (flip f) z (reverse qs)

    back qs = case front (reverse qs) of
      Just (Tuple q qs') -> Just (Tuple q (reverse qs'))
      Nothing -> Nothing

    reverse EmptyQueue = EmptyQueue
    reverse (Enqueue q qs) = reverse qs <> singleton q
