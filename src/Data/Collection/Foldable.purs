module Data.Collection.Foldable where

  import Data.Array ((:))
  import Data.Maybe

    -- | This differs from the standard `Foldable`
  --   in that it doesn't attempt to impose an ordering.
  --   Specifically, there is no notion of `foldl` or `foldr`
  --
  --   It intentionally conflict with the standard `Foldable`.
  class Foldable f a where
    fold :: forall b. (a -> b -> b) -> b -> f a -> b

  instance foldableArray :: Foldable [] a where
    fold _ z [] = z
    fold f z (x:xs) = x `f` (fold f z xs)

  instance foldableMaybe :: Foldable Maybe a where
    fold _ z Nothing = z
    fold f z (Just x) = x `f` z

  size :: forall a f. (Foldable f a) => f a -> Number
  size fs = fold (const ((+) 1)) 0 fs

  isEmpty :: forall a f. (Foldable f a) => f a -> Boolean
  isEmpty fs = size fs == 0

  isSingleton :: forall a f. (Foldable f a) => f a -> Boolean
  isSingleton fs = size fs == 1

  and :: forall f. (Foldable f) => f Boolean -> Boolean
  and fs = all id fs

  or :: forall f. (Foldable f) => f Boolean -> Boolean
  or fs = any id fs

  all :: forall a f. (Foldable f a) => (a -> Boolean) -> f a -> Boolean
  all p fs = fold ((&&) <<< p) true fs

  any :: forall a f. (Foldable f a) => (a -> Boolean) -> f a -> Boolean
  any p fs = fold ((||) <<< p) false fs

  sum :: forall f. (Foldable f) => f Number -> Number
  sum fs = fold (+) 0 fs

  product :: forall f. (Foldable f) => f Number -> Number
  product fs = fold (*) 1 fs

  elem :: forall a f. (Eq a, Foldable f a) => a -> f a -> Boolean
  elem f fs = any ((==) f) fs

  notElem :: forall a f. (Eq a, Foldable f a) => a -> f a -> Boolean
  notElem f fs = not (elem f fs)

  contains :: forall a f. (Eq a, Foldable f a) => f a -> f a -> Boolean
  contains cs cs' = all (\c ->  c `elem` cs) cs'
