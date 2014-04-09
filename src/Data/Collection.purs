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

  -- | This differs from the standard Foldable
  --   in that it doesn't attempt to impose an ordering.
  --   specifically, there is no notion of `foldl` or `foldr`
  class Foldable f where
    -- `fold` is the minimum required to implement a `Foldable`.
    fold :: forall a b. (a -> b -> b) -> b -> f a -> b
    -- `size` should have a default implementation.
    size :: forall a. f a -> Number
    -- size = fold (const ((+) 1)) 0

  -- | An unordered collection of things.
  class (Foldable c, Monoid (c a)) <= Collection c a where
    -- cons` is the minimum required to implement a `Collection`.
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

    size = fold (const ((+) 1)) 0

  instance collectionArray :: Collection [] a where
    cons = (:)

    filter p xs = fold (\x acc -> if p x then cons x acc else acc) mempty xs
    partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)

  instance sequenceArray :: Sequence [] a where
    foldl _ z [] = z
    foldl f z (x:xs) = foldl f (z `f` x) xs

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

    size = fold (const ((+) 1)) 0

  instance semigroupMaybe :: (Semigroup a) => Semigroup (Maybe a) where
    (<>) (Just x) (Just y) = Just (x <> y)
    (<>) _        _        = Nothing

  instance monoidMaybe :: (Monoid a) => Monoid (Maybe a) where
    mempty = Nothing

  instance collectionMaybe :: (Monoid a) => Collection Maybe a where
    cons x Nothing = Just x
    cons x (Just y) = Just (x <> y)

    filter p xs = fold (\x acc -> if p x then cons x acc else acc) mempty xs
    partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)

  instance sequenceMaybe :: (Monoid a) => Sequence Maybe a where
    foldl _ z Nothing = z
    foldl f z (Just x) = z `f` x

    first Nothing = SeqNull
    first (Just x) = SeqCons x Nothing

    last x = first x

    snoc Nothing x = Just x
    snoc (Just x) y = Just (x <> y)

    reverse = id

  -- Tree

  instance foldableTree :: Foldable Tree where
    fold _ z _  = z
    fold f z (Leaf x) = x `f` z
    fold f z (Branch l r) = fold f (fold f z l) r

    size = fold (const ((+) 1)) 0

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

    filter p xs = fold (\x acc -> if p x then cons x acc else acc) mempty xs
    partition p xs = Tuple (filter p xs) (filter (not <<< p) xs)
