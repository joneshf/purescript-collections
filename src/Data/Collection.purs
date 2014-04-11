module Data.Collection where

  import Data.Array ((:))
  import Data.Collection.Foldable
  import Data.Maybe
  import Data.Monoid
  import Data.Tuple

  -- | An unordered collection of things.
  --   One of either `add` or `singleton` is required.
  class (Foldable c a, Monoid (c a)) <= Collection c a where
    add :: a -> c a -> c a
    -- add x xs = singleton x <> xs

    singleton :: a -> c a
    -- singleton x = add x mempty

    -- (<>) :: c a -> c a -> c a
    -- (<>) (add x xs) ys = add x (xs <> ys)

  -- Array

  instance collectionArray :: Collection [] a where
    add = (:)
    singleton x = [x]

  -- Maybe

  instance semigroupMaybe :: (Semigroup a) => Semigroup (Maybe a) where
    (<>) Nothing  m        = m
    (<>) m        Nothing  = m
    (<>) (Just x) (Just y) = Just (x <> y)

  instance monoidMaybe :: (Monoid a) => Monoid (Maybe a) where
    mempty = Nothing

  instance collectionMaybe :: (Monoid a) => Collection Maybe a where
    add x Nothing = Just x
    add x (Just y) = Just (x <> y)
    singleton x = Just x

  remove :: forall a c. (Eq a, Collection c a) => a -> c a -> c a
  remove c cs = filter ((/=) c) cs

  filter :: forall a c. (Collection c a) => (a -> Boolean) -> c a -> c a
  filter p cs = fold (\c acc -> if p c then add c acc else acc) mempty cs

  partition :: forall a c. (Collection c a) => (a -> Boolean) -> c a -> Tuple (c a) (c a)
  partition p cs = fold go (Tuple mempty mempty) cs
    where
      go c (Tuple pass fail) | p c = Tuple (add c pass) fail
      go c (Tuple pass fail)       = Tuple pass (add c fail)

  toArray :: forall a c. (Collection c a) => c a -> [a]
  toArray cs = fold (:) [] cs

  fromArray :: forall a c. (Collection c a) => [a] -> c a
  fromArray xs = fold ((<>) <<< singleton) mempty xs
