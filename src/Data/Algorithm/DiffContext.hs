-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithm.DiffContext
-- Copyright   :  (c) David Fox (2015)
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- Author      :  David Fox (ddssff at the email service from google)
--
-- Generates a grouped diff with merged runs, and outputs them in the manner of @diff -u@.
-----------------------------------------------------------------------------
module Data.Algorithm.DiffContext
    ( ContextDiff, Hunk
    , getContextDiff
    , prettyContextDiff
    , prettyContextDiffOld
    , getContextDiffNumbered
    , Numbered(Numbered), numbered, unnumber
    , unNumberContextDiff
    , groupBy'
    ) where

import Data.Algorithm.Diff (PolyDiff(..), Diff, getGroupedDiff)
import Data.Bifunctor
import Text.PrettyPrint (Doc, text, empty, hcat)

-- | A diff consisting of disjoint 'Hunk's.
type ContextDiff c = [Hunk c]

-- | A 'Hunk' is a list of adjacent 'Diff's.
type Hunk c = [Diff [c]]


-- | Groups elements so that consecutive elements in a group satisfy the predicate.
-- This is unlike 'Data.List.groupBy' where grouped elements are only guaranteed to
-- satisfy the predicate w.r.t. the first element of the group.
--
-- For instance, to split the input where there are two consecutive `1`s:
--
--     > let notBoth1 a b = not (a == 1 && b == 1) in
--     >
--     > groupBy' notBoth1 [1,1,2,3,1,1,4,5,6,1]
--     > [[1],[1,2,3,1],[1,4,5,6,1]]
--     >
--     > groupBy notBoth1 [1,1,2,3,1,1,4,5,6,1]
--     > [[1],[1,2,3],[1],[1,4,5,6],[1]]
--
-- In the first result the list is split anywhere there are two
-- adjacent ones, as desired.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' eq (x0 : xs0) = go [x0] xs0
    where
      go (x : xs) (y : zs) | eq x y = go (y : x : xs) zs
      go g (y : zs) = reverse g : go [y] zs
      go g [] = [reverse g]

data Numbered a = Numbered Int a deriving Show
instance Eq a => Eq (Numbered a) where
  Numbered _ a == Numbered _ b = a == b
instance Ord a => Ord (Numbered a) where
  compare (Numbered _ a) (Numbered _ b) = compare a b

numbered :: [a] -> [Numbered a]
numbered xs = fmap (uncurry Numbered) (zip [1..] xs)

unnumber :: Numbered a -> a
unnumber (Numbered _ a) = a

-- |
-- > > let textA = ["a","b","c","d","e","f","g","h","i","j","k"]
-- > > let textB = ["a","b","d","e","f","g","h","i","j"]
-- > > let diff = getContextDiff (Just 2) textA textB
-- > > prettyContextDiff (text "file1") (text "file2") (text . unnumber) diff
-- > --- file1
-- > +++ file2
-- > @@ -1,5 +1,4 @@
-- >  a
-- >  b
-- > -c
-- >  d
-- >  e
-- > @@ -9,3 +8,2 @@
-- >  i
-- >  j
-- > -k
getContextDiff ::
  Eq a
  => Maybe Int -- ^ Number of context elements, 'Nothing' means returning a whole-diff 'Hunk'.
  -> [a]
  -> [a]
  -> ContextDiff (Numbered a)
getContextDiff context a b =
  getContextDiffNumbered context (numbered a) (numbered b)

-- | If for some reason you need the line numbers stripped from the
-- result of 'getContextDiff' for backwards compatibility.
unNumberContextDiff :: ContextDiff (Numbered a) -> ContextDiff a
unNumberContextDiff = fmap (fmap (bimap (fmap unnumber) (fmap unnumber)))

-- | Create a diff made of separate 'Hunk's by reducing the lists of common
-- elements surrounding each sequence of differing elements to the specified
-- @context@ number. Adjancent hunks end up merged if the list of common elements
-- between them is shorter than twice the @context@.
-- If @context@ is 'Nothing', we get a single hunk with the whole diff.
getContextDiffNumbered ::
  Eq a
  => Maybe Int -- ^ Number of context elements, 'Nothing' means returning a whole-diff 'Hunk'.
  -> [Numbered a]
  -> [Numbered a]
  -> ContextDiff (Numbered a)
getContextDiffNumbered context a0 b0 =
    -- The 'Diff' list is grouped into 'Hunks' that begin and end
    -- with matching ('Both') text, having non-matching ('First' and 'Second')
    -- text in the middle. Note that a non-trivial partition can only happen after
    -- the matching text has been reduced to become consecutive 'Both' values
    -- corresponding to a hunk's suffix and the following hunk prefix.
    groupBy' (\a b -> not (isBoth a && isBoth b)) $ doPrefix $ getGroupedDiff a0 b0
    where
      isBoth (Both _ _) = True
      isBoth _ = False
      -- | Handle the common text leading up to a diff.
      doPrefix :: Hunk a -> Hunk a
      doPrefix [] = []
      -- Trailing common elements are no prefix.
      doPrefix [Both _ _] = []
      -- Do the prefix proper.
      doPrefix (Both xs ys : more) =
          Both (maybe xs (\n -> drop (max 0 (length xs - n)) xs) context)
               (maybe ys (\n -> drop (max 0 (length ys - n)) ys) context) : doSuffix more
      -- Prefix finished, do the diff then the following suffix.
      doPrefix (d : ds) = doSuffix (d : ds)
      -- | Handle the common text following a diff.
      doSuffix :: Hunk a -> Hunk a
      doSuffix [] = []
      -- A trailing suffix.
      doSuffix [Both xs ys] = [Both (maybe xs (\n -> take n xs) context) (maybe ys (\n -> take n ys) context)]
      -- Infinite context or common text too short to split.
      doSuffix (Both xs ys : more)
          | maybe True (\n -> length xs <= n * 2) context =
              Both xs ys : doPrefix more
      -- If the common text long enough, split it into a suffix and prefix
      -- (resulting in some elements excluded from the diff in the middle).
      doSuffix (Both xs ys : more) =
          Both (maybe xs (\n -> take n xs) context) (maybe ys (\n -> take n ys) context)
                   -- NOTE: both 'mempty's here are unreachable in practice because:
                   -- 1. The guard above ensures that @context@ is not 'Nothing'
                   -- 2. Both lists have the same length.
                   : doPrefix (Both (maybe mempty (\n -> drop n xs) context) (maybe mempty (\n -> drop n ys) context) : more)
      -- Diff elements are preserved.
      doSuffix (d : ds) = d : doSuffix ds

-- | Pretty print a ContextDiff in the manner of diff -u.
prettyContextDiff ::
       Doc            -- ^ Document 1 name
    -> Doc            -- ^ Document 2 name
    -> (Numbered c -> Doc)     -- ^ Element pretty printer
    -> ContextDiff (Numbered c)
    -> Doc
prettyContextDiff _ _ _ [] = empty
prettyContextDiff old new prettyElem hunks =
    hcat . map (<> text "\n") $ (text "--- " <> old :
                                 text "+++ " <> new :
                                 concatMap prettyRun hunks)
    where
      -- Pretty print a run of adjacent changes
      prettyRun hunk =
        text ("@@ " <> formatHunk hunk <> " @@") : concatMap prettyChange hunk

      -- Pretty print a single change (e.g. one line of a text file)
      prettyChange (Both ts _) = map (\ l -> text " " <> prettyElem l) ts
      prettyChange (First ts)  = map (\ l -> text "-" <> prettyElem l) ts
      prettyChange (Second ts) = map (\ l -> text "+" <> prettyElem l) ts

      formatHunk hunk = "-" <> formatRun (firsts hunk) <> " +" <> formatRun (seconds hunk)

      formatRun :: [Int] -> String
      formatRun [] = "-0,0"
      formatRun [n] = show n
      formatRun ns@(n : _) = show n <> "," <> show (length ns)

      firsts (Both ns _ : more) = fmap (\(Numbered n _) -> n) ns <> firsts more
      firsts (First ns : more) = fmap (\(Numbered n _) -> n) ns <> firsts more
      firsts (Second _ : more) = firsts more
      firsts [] = []

      seconds (Both _ ns : more) = fmap (\(Numbered n _) -> n) ns <> seconds more
      seconds (First _ : more) = seconds more
      seconds (Second ns : more) = fmap (\(Numbered n _) -> n) ns <> seconds more
      seconds [] = []

-- | Pretty print without line numbers.
prettyContextDiffOld ::
       Doc            -- ^ Document 1 name
    -> Doc            -- ^ Document 2 name
    -> (c -> Doc)     -- ^ Element pretty printer
    -> ContextDiff c
    -> Doc
prettyContextDiffOld _ _ _ [] = empty
prettyContextDiffOld old new prettyElem hunks =
    hcat . map (<> text "\n") $ (text "--- " <> old :
                                 text "+++ " <> new :
                                 concatMap prettyRun hunks)
    where
      -- Pretty print a run of adjacent changes
      prettyRun hunk =
          text "@@" : concatMap prettyChange hunk

      -- Pretty print a single change (e.g. one line of a text file)
      prettyChange (Both ts _) = map (\ l -> text " " <> prettyElem l) ts
      prettyChange (First ts)  = map (\ l -> text "-" <> prettyElem l) ts
      prettyChange (Second ts) = map (\ l -> text "+" <> prettyElem l) ts
