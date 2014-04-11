module Data.Collection.Stack where

  import Data.Collection
  import Data.Collection.Foldable
  import Data.Collection.Sequence

  import Data.Maybe
  import Data.Monoid
  import Data.Tuple

  data Stack a = EmptyStack
               | Push a (Stack a)

  instance showStack :: (Show a) => Show (Stack a) where
    show EmptyStack = "EmptyStack"
    show (Push s ss) = "Stack(" ++ show s ++ ", " ++ show ss ++ ")"

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
