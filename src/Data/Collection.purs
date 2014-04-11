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

  -- This should have O(1) insert and removal.
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
  class Foldable f a where
    fold :: forall b. (a -> b -> b) -> b -> f a -> b

  -- | An unordered collection of things.
  --   One of either `add` or `singleton` is required.
  class (Foldable c a, Monoid (c a)) <= Collection c a where
    add :: a -> c a -> c a
    -- add x xs = singleton x <> xs

    singleton :: a -> c a
    -- singleton x = add x mempty

    -- (<>) :: c a -> c a -> c a
    -- (<>) (add x xs) ys = add x (xs <> ys)

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

  -- Array

  instance foldableArray :: Foldable [] a where
    fold _ z [] = z
    fold f z (x:xs) = x `f` (fold f z xs)

  instance collectionArray :: Collection [] a where
    add = (:)
    singleton x = [x]

  instance sequenceArray :: Sequence [] a where
    cons = (:)
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

  -- Maybe

  instance foldableMaybe :: Foldable Maybe a where
    fold _ z Nothing = z
    fold f z (Just x) = x `f` z

  instance semigroupMaybe :: (Semigroup a) => Semigroup (Maybe a) where
    (<>) Nothing  m        = m
    (<>) m        Nothing  = m
    (<>) (Just x) (Just y) = Just (x <> y)

  instance monoidMaybe :: (Monoid a) => Monoid (Maybe a) where
    mempty = Nothing

  instance collectionMaybe :: (Monoid a) => Collection Maybe a where
    add m ms = cons m ms
    singleton x = Just x

  instance sequenceMaybe :: (Monoid a) => Sequence Maybe a where
    cons x Nothing = Just x
    cons x (Just y) = Just (x <> y)

    snoc Nothing x = Just x
    snoc (Just x) y = Just (x <> y)

    foldl f z xs = fold (flip f) z xs

    foldr f z xs = fold f z xs

    front Nothing = Nothing
    front (Just x) = Just (Tuple x Nothing)

    back x = front x

    reverse xs = xs

  -- Tree

  instance foldableTree :: Foldable Tree a where
    fold _ z Bud  = z
    fold f z (Leaf x) = x `f` z
    fold f z (Branch l r) = fold f (fold f z l) r

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
    add t ts = singleton t <> ts
    singleton x = Leaf x

  -- Stack

  instance foldableStack :: Foldable Stack a where
    fold _ z EmptyStack = z
    fold f z (Push s ss) = s `f` (fold f z ss)

  instance semigroupStack :: Semigroup (Stack a) where
    (<>) EmptyStack s = s
    (<>) s EmptyStack = s
    (<>) (Push s ss) ss' = Push s (ss <> ss')

  instance monoidStack :: Monoid (Stack a) where
    mempty = EmptyStack

  instance collectionStack :: Collection Stack a where
    add s ss = Push s ss
    singleton x = Push x EmptyStack

  instance sequenceStack :: Sequence Stack a where
    cons s ss = Push s ss

    foldr f z ss = fold f z ss

    front EmptyStack = Nothing
    front (Push s ss) = Just (Tuple s ss)

    -- Use default implementations.
    snoc ss s = reverse (cons s (reverse ss))

    foldl f z ss = foldr (flip f) z (reverse ss)

    back ss = case front (reverse ss) of
      Just (Tuple s ss') -> Just (Tuple s (reverse ss'))
      Nothing -> Nothing

    reverse EmptyStack = EmptyStack
    reverse (Push s ss) = reverse ss <> singleton s

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

  -- Derivable combinators

  -- Foldable

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

  -- Collection

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

  -- Sequence

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
