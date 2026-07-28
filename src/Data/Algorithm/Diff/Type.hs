-- | Basic diff type and instances.
module Data.Algorithm.Diff.Type where

import Data.Bifunctor

-- | A value tagged with which of two input sequences it came from.
-- The type parameters @a@ and @b@ may differ, which is useful when comparing
-- sequences of different element types via a custom equality predicate.
--
-- Each constructor corresponds to one outcome for a position in the aligned
-- sequences:
--
-- * 'First' — the element exists only in the /first/ input (a deletion).
-- * 'Second' — the element exists only in the /second/ input (an insertion).
-- * 'Both' — the element is common to both inputs.
--   Both the left and right values are retained so that the original
--   elements can be recovered even when equality ignores some fields.
{-@ data PolyDiff a b = First a | Second b | Both a b @-}
data PolyDiff a b = First a | Second b | Both a b
    deriving (Show, Eq)

instance Functor (PolyDiff a) where
  fmap _ (First a) = First a
  fmap g (Second b) = Second (g b)
  fmap g (Both a b) = Both a (g b)

instance Bifunctor PolyDiff where
  bimap f _ (First a) = First (f a)
  bimap _ g (Second b) = Second (g b)
  bimap f g (Both a b) = Both (f a) (g b)

-- | This is 'PolyDiff' specialized so both sides are the same type.
type Diff a = PolyDiff a a
