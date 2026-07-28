-- | Refinement type aliases for 'PolyDiff', related predicates and (lifted) utility functions.
-- The contents of this module are intended for use in Liquid Haskell specifications;
-- importing this module can result in unused import warnings in GHC,
-- which can be suppressed with @-Wno-unused-imports@.
module Data.Algorithm.Diff.Refinement where

import Data.Algorithm.Diff.Type

-- * Diff predicates and refinement type aliases

{-@
inline validListDiff
define length x = len x
@-}
-- | True when, for a 'Both' value, both sides have the same length.
-- 'First' and 'Second' trivially satisfy this.
validListDiff :: PolyDiff [a] [b] -> Bool
validListDiff (Both xs ys) = length xs == length ys
validListDiff (First _) = True
validListDiff (Second _) = True

{-@ inline nonEmptyDiff @-}
-- | True when the diff is not empty on either side.
nonEmptyDiff :: PolyDiff [a] [b] -> Bool
nonEmptyDiff (First []) = False
nonEmptyDiff (Second []) = False
nonEmptyDiff (Both [] _) = False
nonEmptyDiff (Both _ []) = False
nonEmptyDiff _ = True

-- A valid list diff is such that any `Both` value has arguments of equal length.
{-@ type ValidListDiff a b = { d : PolyDiff [a] [b] | validListDiff d }@-}

-- Non-empty valued valid list diffs, used for grouped diffs.
{-@ type GroupedDiff a b = { d : ValidListDiff a b | nonEmptyDiff d } @-}

{-@ reflect headIsFirst @-}
{-@ reflect headIsSecond @-}
{-@ reflect headIsBoth @-}
-- | Head-constructor predicates for 'PolyDiff' lists.
-- Reflected (not measures) to avoid sort errors: measures on @[PolyDiff a b]@
-- would be attached to the polymorphic @[]@ constructor, clashing with
-- lists of other element types.
headIsFirst, headIsSecond, headIsBoth :: [PolyDiff a b] -> Bool
headIsFirst (First _ : _) = True
headIsFirst _ = False
headIsSecond (Second _ : _) = True
headIsSecond _ = False
headIsBoth (Both _ _ : _) = True
headIsBoth _ = False

{-@ reflect noStuttering @-}
-- | True if the list does not contain adjacent 'Diff's of the same type.
-- Uses head-constructor measures so PLE can work with opaque tails.
noStuttering :: [PolyDiff a b] -> Bool
noStuttering [] = True
noStuttering (First _ : xs) = not (headIsFirst xs) && noStuttering xs
noStuttering (Second _ : xs) = not (headIsSecond xs) && noStuttering xs
noStuttering (Both _ _ : xs) = not (headIsBoth xs) && noStuttering xs

{-@ reflect noFFSS @-}
-- | Like 'noStuttering' but allows Both-Both adjacencies.
-- This is the invariant preserved by @doPrefix@\/@doSuffix@ which may split
-- a single 'Both' into two consecutive 'Both' elements.
noFFSS :: [PolyDiff a b] -> Bool
noFFSS [] = True
noFFSS (First _ : xs) = not (headIsFirst xs) && noFFSS xs
noFFSS (Second _ : xs) = not (headIsSecond xs) && noFFSS xs
noFFSS (Both _ _ : xs) = noFFSS xs

-- * Lifted utility functions

-- | Measures for triplet projections.
{-@
measure fst3
measure snd3
measure thd3
@-}
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z
