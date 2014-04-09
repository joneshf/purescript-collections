module Data.Collection where

  import Data.Array ((:))
  import Data.Maybe
  import Data.Monoid
  import Data.Tuple

  data SeqView s a = SeqNull
                   | SeqCons a (s a)

  instance showSeqView :: (Show a, Show (s a)) => Show (SeqView s a) where
    show SeqNull = "SeqNull"
    show (SeqCons x xs) = "SeqCons(" ++ show x ++ ", " ++ show xs ++ ")"

  -- Leafy tree
  data Tree a = Bud
              | Leaf a
              | Branch (Tree a) (Tree a)

  instance showTree :: (Show a) => Show (Tree a) where
    show Bud = "Bud"
    show (Leaf x) = "Leaf(" ++ show x ++ ")"
    show (Branch l r) = "Branch(" ++ show l ++ ", " ++ show r ++ ")"

  data Stack a = EmptyStack
               | Push a (Stack a)

  instance showStack :: (Show a) => Show (Stack a) where
    show EmptyStack = "EmptyStack"
    show (Push s ss) = "Stack(" ++ show s ++ ", " ++ show ss ++ ")"

  data Queue a = EmptyQueue
               | Enqueue a (Queue a)

  instance showQueue :: (Show a) => Show (Queue a) where
    show EmptyQueue = "EmptyQueue"
    show (Enqueue q qs) = "Queue(" ++ show q ++ ", " ++ show qs ++ ")"

  -- | This differs from the standard `Foldable`
  --   in that it doesn't attempt to impose an ordering.
  --   Specifically, there is no notion of `foldl` or `foldr`
  --
  --   It intentionally conflict with the standard `Foldable`.
  --
  --   Minimum definition needs `fold`.
  class Foldable f where
    fold :: forall a b. (a -> b -> b) -> b -> f a -> b
    -- `size` should have a default implementation.
    size :: forall a. f a -> Number
    -- size = fold (const ((+) 1)) 0

  -- | An unordered collection of things.
  --   Minimum definition needs `cons`.
  class (Foldable c, Monoid (c a)) <= Collection c a where
    cons :: a -> c a -> c a

    -- `filter` and `partition` should have default implementations.
    filter :: (a -> Boolean) -> c a -> c a
    -- filter p xs = fold (\x acc -> if p x then cons x acc else acc) unit xs
    partition :: (a -> Boolean) -> c a -> Tuple (c a) (c a)
    -- partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)

  -- | Collection with some sense of order.
  --   Minimum definition needs one of `foldl` or `foldr`,
  --   and one of `first` or `last`.
  class (Collection s a) <= Sequence s a where
    foldl :: forall b. (a -> b -> a) -> a -> s b -> a
    -- foldl f z xs = foldr (flip f) z (reverse xs)
    -- `foldr` has the same type as `fold`,
    -- but enforces an ordering on the evaluation.
    foldr :: forall b. (a -> b -> b) -> b -> s a -> b
    -- foldr f z xs = foldl (flip f) z (reverse xs)
    first :: s a -> SeqView s a
    -- first xs = case last (reverse xs) of
    --   SeqCons x xs' -> SeqCons x (reverse xs')
    --   SeqNull -> SeqNull
    last :: s a -> SeqView s a
    -- last xs = case first (reverse xs) of
    --   SeqCons x xs' -> SeqCons x (reverse xs')
    --   SeqNull -> SeqNull
    reverse :: s a -> s a
    -- reverse xs = foldl (flip cons) mempty xs
    snoc :: s a -> a -> s a
    -- snoc xs x = reverse (cons x (reverse xs))

  -- Array

  instance foldableArray :: Foldable [] where
    fold _ z [] = z
    fold f z (x:xs) = x `f` (fold f z xs)

    size xs = fold (const ((+) 1)) 0 xs

  instance collectionArray :: Collection [] a where
    cons = (:)

    filter p xs = fold (\x acc -> if p x then cons x acc else acc) mempty xs
    partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)

  instance sequenceArray :: Sequence [] a where
    foldl _ z [] = z
    foldl f z (x:xs) = foldl f (z `f` x) xs

    foldr f z xs = fold f z xs

    first [] = SeqNull
    first (x:xs) = SeqCons x xs

    last xs = case first (reverse xs) of
      SeqCons x xs' -> SeqCons x (reverse xs')
      SeqNull -> SeqNull

    snoc xs x = xs ++ [x]

    reverse = Data.Array.reverse

  -- Maybe

  instance foldableMaybe :: Foldable Maybe where
    fold _ z Nothing = z
    fold f z (Just x) = x `f` z

    size Nothing = 0
    size (Just _) = 1

  instance semigroupMaybe :: (Semigroup a) => Semigroup (Maybe a) where
    (<>) (Just x) (Just y) = Just (x <> y)
    (<>) _        _        = Nothing

  instance monoidMaybe :: (Monoid a) => Monoid (Maybe a) where
    mempty = Nothing

  instance collectionMaybe :: (Monoid a) => Collection Maybe a where
    cons x Nothing = Just x
    cons x (Just y) = Just (x <> y)

    -- Use default implementations.
    filter p xs = fold (\x acc -> if p x then cons x acc else acc) mempty xs
    partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)

  instance sequenceMaybe :: (Monoid a) => Sequence Maybe a where
    foldl _ z Nothing = z
    foldl f z (Just x) = z `f` x

    foldr f z xs = fold f z xs

    first Nothing = SeqNull
    first (Just x) = SeqCons x Nothing

    last x = first x

    snoc Nothing x = Just x
    snoc (Just x) y = Just (x <> y)

    reverse = id

  -- Tree

  instance foldableTree :: Foldable Tree where
    fold _ z Bud  = z
    fold f z (Leaf x) = x `f` z
    fold f z (Branch l r) = fold f (fold f z l) r

    -- Use default implementation.
    size xs = fold (const ((+) 1)) 0 xs

  -- Using a `Tree` as a `Semigroup` only makes sense if we ignore
  -- the structure of the tree, and focus on the semantics of a tree.
  -- Semantic equality is necessary for the laws to hold.
  -- This will work fine for `Collection`, but probably not so much for `Sequence`.
  instance semigroupTree :: Semigroup (Tree a) where
    (<>) Bud t   = t
    (<>) t   Bud = t
    (<>) l   r   = Branch l r

  instance monoidTree :: Monoid (Tree a) where
    mempty = Bud

  instance collectionTree :: Collection Tree a where
    cons x t = Leaf x <> t

    -- Use default implementations.
    filter p xs = fold (\x acc -> if p x then cons x acc else acc) mempty xs
    partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)

  -- Stack

  instance foldableStack :: Foldable Stack where
    fold _ z EmptyStack = z
    fold f z (Push s ss) = s `f` (fold f z ss)

    -- Use default implementation.
    size ss = fold (const ((+) 1)) 0 ss

  instance semigroupStack :: Semigroup (Stack a) where
    (<>) EmptyStack s = s
    (<>) s EmptyStack = s
    (<>) (Push s ss) ss' = Push s (ss <> ss')

  instance monoidStack :: Monoid (Stack a) where
    mempty = EmptyStack

  instance collectionStack :: Collection Stack a where
    cons s ss = Push s ss

    -- Use default implementations.
    filter p xs = fold (\x acc -> if p x then cons x acc else acc) mempty xs
    partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)

  instance sequenceStack :: Sequence Stack a where
    foldl f z ss = fold (flip f) z ss

    last EmptyStack = SeqNull
    last (Push s ss) = SeqCons s ss

    -- Use default implementations.
    foldr f z ss = foldl (flip f) z (reverse ss)
    first ss = case last (reverse ss) of
      SeqCons s ss' -> SeqCons s (reverse ss')
      SeqNull -> SeqNull
    reverse ss = foldl (flip cons) mempty ss
    snoc ss s = reverse (cons s (reverse ss))

  -- Queue

  instance foldableQueue :: Foldable Queue where
    fold _ z EmptyQueue = z
    fold f z (Enqueue q qs) = q `f` (fold f z qs)

    -- Use default implementation.
    size ss = fold (const ((+) 1)) 0 ss

  instance semigroupQueue :: Semigroup (Queue a) where
    (<>) EmptyQueue q = q
    (<>) q EmptyQueue = q
    (<>) (Enqueue q qs) qs' = Enqueue q (qs <> qs')

  instance monoidQueue :: Monoid (Queue a) where
    mempty = EmptyQueue

  instance collectionQueue :: Collection Queue a where
    cons q qs = Enqueue q qs

    -- Use default implementations.
    filter p xs = fold (\x acc -> if p x then cons x acc else acc) mempty xs
    partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)

  instance sequenceQueue :: Sequence Queue a where
    foldl f z qs = fold (flip f) z qs

    first EmptyQueue = SeqNull
    first (Enqueue q qs) = SeqCons q qs

    -- Use default implementations.
    foldr f z qs = foldl (flip f) z (reverse qs)
    last qs = case first (reverse qs) of
      SeqCons q qs' -> SeqCons q (reverse qs')
      SeqNull -> SeqNull
    reverse qs = foldl (flip cons) mempty qs
    snoc qs q = reverse (cons q (reverse qs))
