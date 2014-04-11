module Data.Collection.Sequence
  ( Sequence
  , cons
  , snoc
  , foldl
  , foldr
  , front
  , back
  , reverse
  , (<:)
  , (:>)
  , head
  , tail
  , last
  , init
  , take
  , drop
  , splitAt
  , isSorted
  , sort
  , sortBy
  ) where

  import Data.Collection
  import Data.Collection.Foldable

  import Data.Maybe
  import Data.Monoid
  import Data.Tuple

    -- | Collection with some sense of order.
  --   Minimum definition needs `reverse` and one of each:
  --   * `cons` or `snoc`
  --   * `foldl` or `foldr`
  --   * `front` or `back`
  class (Collection s a) <= Sequence s a where
    cons :: a -> s a -> s a
    -- cons x xs = reverse (snoc (reverse xs) x)

    snoc :: s a -> a -> s a
    -- snoc xs x = reverse (cons x (reverse xs))

    foldl :: forall b. (b -> a -> b) -> b -> s a -> b
    -- foldl f z xs = foldr (flip f) z (reverse xs)

    -- `foldr` has the same type as `fold`,
    -- but enforces an ordering on the evaluation.
    -- in some cases `foldr` == `fold`, but this cannot be guaranteed.
    foldr :: forall b. (a -> b -> b) -> b -> s a -> b
    -- foldr f z xs = foldl (flip f) z (reverse xs)

    front :: s a -> Maybe (Tuple a (s a))
    -- front xs = case back (reverse xs) of
    --   Just (Tuple x xs') -> Just (Tuple x (reverse xs'))
    --   Nothing -> Nothing

    back :: s a -> Maybe (Tuple a (s a))
    -- back xs = case front (reverse xs) of
    --   Just (Tuple x xs') -> Just (Tuple x (reverse xs'))
    --   Nothing -> Nothing

    reverse :: s a -> s a

  instance sequenceArray :: Sequence [] a where
    cons x xs = add x xs
    snoc xs x = xs ++ [x]

    foldl _ z [] = z
    foldl f z (x:xs) = foldl f (z `f` x) xs

    foldr f z xs = fold f z xs

    front [] = Nothing
    front (x:xs) = Just (Tuple x xs)

    back xs = case front (reverse xs) of
      Just (Tuple x xs') -> Just (Tuple x (reverse xs'))
      Nothing -> Nothing

    reverse xs = Data.Array.reverse xs

  instance sequenceMaybe :: (Monoid a) => Sequence Maybe a where
    cons x xs = add x xs

    snoc Nothing x = Just x
    snoc (Just x) y = Just (x <> y)

    foldl f z xs = fold (flip f) z xs

    foldr f z xs = fold f z xs

    front Nothing = Nothing
    front (Just x) = Just (Tuple x Nothing)

    back x = front x

    reverse xs = xs



  infixr 6 <:
  infixl 6 :>
  infixl 8 !

  (<:) :: forall a s. (Sequence s a) => a -> s a -> s a
  (<:) = cons

  (:>) :: forall a s. (Sequence s a) => s a -> a -> s a
  (:>) = snoc

  head :: forall a s. (Sequence s a) => s a -> Maybe a
  head ss = maybe Nothing (Just <<< fst) (front ss)

  tail :: forall a s. (Sequence s a) => s a -> Maybe (s a)
  tail ss = maybe Nothing (Just <<< snd) (front ss)

  last :: forall a s. (Sequence s a) => s a -> Maybe a
  last ss = maybe Nothing (Just <<< fst) (back ss)

  init :: forall a s. (Sequence s a) => s a -> Maybe (s a)
  init ss = maybe Nothing (Just <<< snd) (back ss)

  take :: forall a s. (Sequence s a) => Number -> s a -> s a
  take n ss = fst $ foldl go (Tuple mempty n) ss
    where
      go acc@(Tuple _ n') _ | n' < 1 = acc
      go (Tuple acc n') s = Tuple (acc :> s) (n' - 1)

  drop :: forall a s. (Sequence s a) => Number -> s a -> s a
  drop n ss = fst $ foldl go (Tuple mempty n) ss
    where
      go (Tuple acc n') _ | n' > 0 = Tuple acc (n' - 1)
      go (Tuple acc n') s = Tuple (acc :> s) n'

  splitAt :: forall a s. (Sequence s a) => Number -> s a -> Tuple (s a) (s a)
  splitAt n ss = Tuple (take n ss) (drop n ss)

  isSorted :: forall a s. (Ord a, Sequence s a) => s a -> Boolean
  isSorted ss = fst $ foldl go (Tuple true Nothing) ss
    where
      go (Tuple _      Nothing)     s = Tuple true                  (Just s)
      go (Tuple sorted (Just prev)) s = Tuple (sorted && prev <= s) (Just s)

  {-
    This is a lift from Haskell's `Data.List.sort` and `Data.List.sortby`.

    There might be something simple that I'm missing or
    maybe I've been staring at this for too long,
    but it doesn't seem to typecheck with a `where` clause in the general case.
    Thus, we have to thread the compare function through each function,
    and have these horrendous type signatures.

    Suffice to say, this could be cleaned up greatly.
  -}
  sort :: forall a s. (Foldable s (s a), Ord a, Sequence s a, Sequence s (s a)) => s a -> s a
  sort ss = sortBy compare ss

  sortBy :: forall a s. (Foldable s (s a), Sequence s a, Sequence s (s a)) => (a -> a -> Ordering) -> s a -> s a
  sortBy cmp ss = mergeAll cmp $ sequences cmp ss

  -- This is not safe in the slightest,
  (!) :: forall a s. (Sequence s a) => s a -> Number -> a
  (!) ss n = case head $ drop n ss of Just x -> x

  sequences :: forall a s. (Foldable s (s a), Sequence s a, Sequence s (s a)) => (a -> a -> Ordering) -> s a -> s (s a)
  sequences cmp abss | size abss > 1 =
    let a = abss ! 0
        b = abss ! 1
        ss = drop 2 abss
    in case a `cmp` b of
      GT -> descending cmp b (singleton a) ss
      _  -> ascending  cmp b (add a)       ss
  sequences cmp s = singleton s

  descending :: forall a s. (Foldable s (s a), Sequence s a, Sequence s (s a)) => (a -> a -> Ordering) -> a -> s a -> s a -> s (s a)
  descending cmp a as bs | size bs > 0 =
    let bs' = drop 1 bs
        b = bs ! 0
    in case a `cmp` b of
      GT -> descending cmp b (add a as) bs'
      _  -> add (add a as) (sequences cmp bs')
  descending cmp a as bs = add (add a as) (sequences cmp bs)

  ascending :: forall a s. (Foldable s (s a), Sequence s a, Sequence s (s a)) => (a -> a -> Ordering) -> a -> (s a -> s a) -> s a -> s (s a)
  ascending cmp a as bs | size bs > 0 =
    let bs' = drop 1 bs
        b = bs ! 0
    in case a `cmp` b of
      GT -> add (as (singleton a)) (sequences cmp bs)
      _  -> ascending cmp b (\ys -> as (add a ys)) bs'
  ascending cmp a as bs = add (as (singleton a)) (sequences cmp bs)

  mergeAll :: forall a s. (Foldable s (s a), Sequence s a, Sequence s (s a)) => (a -> a -> Ordering) -> s (s a) -> s a
  mergeAll cmp xs | isSingleton xs = xs ! 0
  mergeAll cmp xs = mergeAll cmp (mergePairs cmp xs)

  mergePairs :: forall a s. (Foldable s (s a), Sequence s a, Sequence s (s a)) => (a -> a -> Ordering) -> s (s a) -> s (s a)
  mergePairs cmp abxs | size abxs > 1 =
    let a = abxs ! 0
        b = abxs ! 1
        xs = drop 2 abxs
    in (merge cmp a b) `add` (mergePairs cmp xs)
  mergePairs cmp xs = xs

  merge :: forall a s. (Sequence s a) => (a -> a -> Ordering) -> s a -> s a -> s a
  merge cmp aas bbs | size aas > 0 && size bbs > 0 =
    let a = aas ! 0
        b = bbs ! 0
        as = drop 1 aas
        bs = drop 1 bbs
    in case a `cmp` b of
      GT -> b `add` merge cmp aas bs
      _  -> a `add` merge cmp as bbs
  merge cmp as bs | isEmpty as = bs
  merge cmp as bs | isEmpty bs = as
