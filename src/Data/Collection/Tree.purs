module Data.Collection.Tree where

  import Data.Collection
  import Data.Collection.Foldable

  import Data.Monoid

  -- Leafy tree
  data Tree a = Bud
              | Leaf a
              | Branch (Tree a) (Tree a)

  instance showTree :: (Show a) => Show (Tree a) where
    show Bud = "Bud"
    show (Leaf x) = "Leaf(" ++ show x ++ ")"
    show (Branch l r) = "Branch(" ++ show l ++ ", " ++ show r ++ ")"

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
