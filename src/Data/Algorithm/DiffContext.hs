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
-- Generates a grouped diff with merged runs, and outputs them in the manner of diff -u
-----------------------------------------------------------------------------
module Data.Algorithm.DiffContext
    ( getContextDiff
    , prettyContextDiff
    ) where

import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import Data.List (groupBy)
import Data.Monoid (mappend)
import Text.PrettyPrint (Doc, text, empty, hcat)

type ContextDiff c = [[Diff [c]]]

-- | See https://github.com/haskell/containers/issues/424
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' eq (x : xs) = go [x] xs
    where
      go (x : xs) (y : zs) | eq x y = go (y : x : xs) zs
      go g (y : zs) = reverse g : go [y] zs
      go g [] = [reverse g]

-- | Do a grouped diff and then split up the chunks into runs that
-- contain differences surrounded by N lines of unchanged text.  If
-- there is less then 2N+1 lines of unchanged text between two
-- changes, the runs are left merged.
getContextDiff :: Eq a => Int -> [a] -> [a] -> ContextDiff a
getContextDiff context a b =
    groupBy' (\a b -> not (isBoth a && isBoth b)) $ doPrefix $ getGroupedDiff a b
    where
      isBoth (Both {}) = True
      isBoth _ = False
      -- Handle the common text leading up to a diff.
      doPrefix [] = []
      doPrefix [Both _ _] = []
      doPrefix (Both xs ys : more) =
          Both (drop (max 0 (length xs - context)) xs)
               (drop (max 0 (length ys - context)) ys) : doSuffix more
      -- Prefix finished, do the diff then the following suffix
      doPrefix (d : ds) = doSuffix (d : ds)
      -- Handle the common text following a diff.
      doSuffix [] = []
      doSuffix [Both xs ys] = [Both (take context xs) (take context ys)]
      doSuffix (Both xs ys : more)
          | length xs <= context * 2 =
              Both xs ys : doPrefix more
      doSuffix (Both xs ys : more) =
          Both (take context xs) (take context ys)
                   : doPrefix (Both (drop context xs) (drop context ys) : more)
      doSuffix (d : ds) = d : doSuffix ds

-- | Pretty print a ContextDiff in the manner of diff -u.
prettyContextDiff ::
       Doc            -- ^ Document 1 name
    -> Doc            -- ^ Document 2 name
    -> (c -> Doc)     -- ^ Element pretty printer
    -> ContextDiff c
    -> Doc
prettyContextDiff _ _ _ [] = empty
prettyContextDiff old new prettyElem hunks =
    hcat . map (`mappend` text "\n") $ (text "--- " `mappend` old :
                                 text "+++ " `mappend` new :
                                 concatMap prettyRun hunks)
    where
      -- Pretty print a run of adjacent changes
      prettyRun hunk =
          text "@@" : concatMap prettyChange hunk

      -- Pretty print a single change (e.g. one line of a text file)
      prettyChange (Both ts _) = map (\ l -> text " " `mappend` prettyElem l) ts
      prettyChange (First ts)  = map (\ l -> text "-" `mappend` prettyElem l) ts
      prettyChange (Second ts) = map (\ l -> text "+" `mappend` prettyElem l) ts
