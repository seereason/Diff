-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithm.Diff
-- Copyright   :  (c) Sterling Clover 2008-2011, Kevin Charter 2011
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This is an implementation of the diff algorithm as described in
-- [/An \( O(ND) \) Difference Algorithm and Its Variations (1986)/
-- by Eugene W. Myers](https://publications.mpi-cbg.de/Myers_1986_6330.pdf).
-- For inputs of size \( O(N) \) with the number of differences \( D \)
-- it has \( O(ND) \) time and \( O(D^2) \) space complexity.
--
-- == Algorithm overview
--
-- Finding the shortest edit script (SES) from a list \( as \) to a list \( bs \)
-- is modelled as a shortest-path search on an /edit graph/: an
-- \( (M+1) \times (N+1) \) grid of nodes \( (i, j) \),
-- where \( M = |as| \) and \( N = |bs| \), with \( i \) increasing rightward
-- and \( j \) increasing downward.
-- Each node represents the state of having consumed \( i \) elements of \( as \)
-- and \( j \) elements of \( bs \). Three types of move are possible:
--
-- * A /rightward/ move \( (i,j) \to (i+1,j) \) represents
--   /deleting/ \( as[i] \) and costs one edit.
-- * A /downward/ move  \( (i,j) \to (i,j+1) \) represents
--   /inserting/ \( bs[j] \) and costs one edit.
-- * A /diagonal/ move  \( (i,j) \to (i+1,j+1) \) is free (zero edit cost)
--   and is only available when \( as[i] = bs[j] \).
--
-- The SES corresponds to a path from \( (0,0) \) to \( (M,N) \) that minimises
-- the number of non-diagonal moves. The nodes at which diagonal moves are taken
-- — the /match points/ — form the Longest Common Subsequence (LCS) of the two
-- input lists, as established in the paper.
--
-- Both input lists are 0-indexed, which leads to a slightly different
-- interpretation of the edit graph than in the original paper. In the paper,
-- each node represents the state of the traversal /after/ an edit, so a move
-- is the edit that /produced/ that node. Here, each node represents the state
-- /before/ an edit, so a move is the edit performed /on/ that node to yield its
-- successor. This distinction is only relevant when reading the implementation
-- alongside the paper.
--
-- === K-diagonals and the BFS frontier
--
-- Every node \( (i,j) \) lies on the /k-diagonal/ \( k = i - j \).
-- After exactly \( D \) non-diagonal moves, every reachable node lies on one of
-- at most \( D+1 \) k-diagonals \( k \in \{-D,\,-D+2,\,\ldots,\,D-2,\,D\} \).
-- On each diagonal it suffices to track only the /furthest-reaching/ node
-- (the one with the largest \( i \)), collapsing the two-dimensional grid to a
-- one-dimensional frontier indexed by \( k \).
--
-- The algorithm performs a BFS over \( D = 0, 1, 2, \ldots \), advancing
-- the frontier by one edit at a time until a frontier node reaches the goal
-- \( (M, N) \). The edit trace stored in that node is the SES, which
-- 'getDiffBy' reconstructs into a 'PolyDiff' list. The term /trace/ here
-- differs from the paper, where it denotes the sequence of k-diagonals visited
-- by the SES path; that structure is not materialised in this implementation.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Data.Algorithm.Diff
    ( Diff, PolyDiff(..)
    -- * Comparing lists for differences
    , getDiff
    , getDiffBy

    -- * Finding chunks of differences
    , getGroupedDiff
    , getGroupedDiffBy
    ) where

import Prelude hiding (pi)
import Data.Array (listArray, (!))
import Data.Bifunctor

-- | /Diff Instruction/ — an internal enum recording the direction of a single
-- non-diagonal edge traversed in the Myers edit graph. Every non-diagonal
-- move in the edit script is one of:
--
-- * 'F' — /First/ — a horizontal edge \( (i,j) \to (i+1,j) \), which
--   corresponds to /deleting/ the element at position \( i \) of the first input
--   sequence. The consumed element appears in the 'First' branch of the
--   resulting 'PolyDiff'.
--
-- * 'S' — /Second/ — a vertical edge \( (i,j) \to (i,j+1) \), which
--   corresponds to /inserting/ the element at position \( j \) of the second
--   input sequence. The consumed element appears in the 'Second' branch of
--   the resulting 'PolyDiff'.
--
-- Diagonal edges (free moves corresponding to equal elements) are /not/
-- recorded as 'DI' steps; they are followed implicitly by 'addsnake' and
-- produce 'Both' entries in the final output.
data DI = F | S deriving (Show, Eq)

-- | A value tagged with which of two input sequences it came from.
-- The type parameters @a@ and @b@ may differ, which is useful when comparing
-- sequences of different element types via a custom equality predicate.
--
-- Each constructor corresponds to one outcome for a position in the aligned
-- sequences:
--
-- * 'First' — the element exists only in the /first/ input (a deletion).
-- * 'Second' — the element exists only in the /second/ input (an insertion).
-- * 'Both' — the element is common to both inputs (part of the LCS).
--   Both the left and right values are retained so that the original
--   elements can be recovered even when equality ignores some fields.
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

-- | /D-path Location/ — a node on the BFS frontier of the Myers O(ND) diff
-- algorithm.
--
-- Each frontier consists of one 'DL' per /k-diagonal/.  A 'DL' stores the
-- endpoint coordinates and the edit trace of a \( D \)-path, i.e. a path from the
-- origin \( (0,0) \) that uses exactly \( D \) non-diagonal edges.
data DL = DL
    { poi  :: !Int   -- ^ /Position On I/ — the @x@-coordinate of the endpoint
                     --   in the edit graph, i.e. the number of elements
                     --   consumed from the /first/ input sequence so far.
    , poj  :: !Int   -- ^ /Position On J/ — the @y@-coordinate of the endpoint
                     --   in the edit graph, i.e. the number of elements
                     --   consumed from the /second/ input sequence so far.
    , path :: [DI]   -- ^ The edit trace accumulated so far, stored in
                     --   /reverse/ order (most recent step first).  Diagonal
                     --   edges (matches) are not recorded here; only 'F' and
                     --   'S' steps are stored.
    } deriving (Show, Eq)

-- | Ordering used by 'dstep' to select the /furthest-reaching/ D-path when
-- two candidates compete for the same k-diagonal.
--
-- As in the Myers algorithm, it is enough to compare by 'poi': the candidate
-- that has advanced further along the \( x \)-axis is the furthest-reaching
-- endpoint on that diagonal.
--
-- When 'poi' values are equal, the instance prefers the node with the
-- smaller 'poj' (equivalently, the higher k-diagonal). In practice this
-- branch is never decisive within 'dstep': competing candidates always
-- share a k-diagonal, so equal 'poi' implies equal 'poj'.
--
-- TODO: This instance is /not/ a lawful 'Ord': it violates reflexivity
-- (@x '<=' x@ is 'False') because the equal-'poi' branch compares 'poj'
-- with a strict @'>'@. This is harmless in the current context, since the
-- only use of this instance is the 'max' call in 'dstep' — which always
-- returns one of its arguments — and when both candidates occupy the same
-- position, either choice is equivalent. This instance should either be
-- made lawful or removed in favour of a local 'max'-like helper.
instance Ord DL
        where x <= y = if poi x == poi y
                then  poj x > poj y
                else poi x <= poi y

-- | Build a /diagonal predicate/ — a closure that tests whether position
-- @(i, j)@ in the edit graph has a diagonal edge (a /match point/ in Myers'
-- terminology).
--
-- Indices are 0-based (\( i \in [0, lena) \), \( j \in [0, lenb) \) ),
-- unlike the 1-based convention of the original paper.
--
-- The first two 'Int' parameters stand for the lengths of the input lists,
-- which are captured from the outer scope to compute them only once.
canDiag :: (a -> b -> Bool) -> [a] -> [b] -> Int -> Int -> Int -> Int -> Bool
canDiag eq as bs lena lenb = \ i j ->
   if i < lena && j < lenb then (arAs ! i) `eq` (arBs ! j) else False
   where
     -- Lists are converted into arrays to have O(1) lookups.
     arAs = listArray (0,lena - 1) as
     arBs = listArray (0,lenb - 1) bs

-- | Perform one BFS expansion step, advancing every frontier 'DL' node by one
-- edit (one non-diagonal edge) and then following any available snake.
--
-- For each existing frontier node the step produces two candidate successors:
--
-- * An 'F' (delete) move: 'poi' incremented by 1.
-- * An 'S' (insert) move: 'poj' incremented by 1.
--
-- 'addsnake' is applied to each candidate immediately to extend it along any
-- available sequence of matching elements.
--
-- The resulting candidate list interleaves the 'F' and 'S' successors of each
-- frontier node. The head ('F' successor of the first node) is kept as-is, and
-- 'pairMaxes' is applied to the tail — pairing each 'S' successor with the 'F'
-- successor of the next frontier node. When this function is iterated from a
-- single-node seed (as in 'lcs'), each such pair always lies on the same
-- diagonal: an 'F' edge advances to the next higher diagonal while an 'S' edge
-- retreats to the next lower one, so the two members of each pair straddle the
-- same diagonal from opposite sides.
dstep
  :: (Int -> Int -> Bool) -- ^ Diagonal predicate
  -> [DL]                 -- ^ Frontier of D-paths at edit distance D
  -> [DL]                 -- ^ Frontier of D-paths at edit distance D+1
dstep cd dls = hd:pairMaxes rst
  where (hd:rst) = nextDLs dls
        -- Extend each frontier node by one edit step in both possible directions
        -- and then follow any available snake from the resulting position.
        nextDLs [] = []
        nextDLs (dl:rest) = dl':dl'':nextDLs rest
          where dl'  = addsnake cd $ dl {poi=poi dl + 1, path=(F : pdl)}
                dl'' = addsnake cd $ dl {poj=poj dl + 1, path=(S : pdl)}
                pdl = path dl
        -- Merge adjacent pairs of candidates to retain only the furthest-reaching.
        pairMaxes [] = []
        pairMaxes [x] = [x]
        pairMaxes (x:y:rest) = max x y:pairMaxes rest

-- | Follow a /snake/ from the current position of a 'DL' node.
--
-- A snake is a sequence of diagonal (cost-free) edges in the edit graph,
-- i.e. a run of equal elements that can be consumed simultaneously
-- from both input sequences without counting as an edit.  Starting from
-- @(poi dl, poj dl)@, this function advances both 'poi' and 'poj' as long
-- as consecutive elements match, leaving 'path' unchanged (diagonal moves
-- are not recorded as edit steps).
addsnake :: (Int -> Int -> Bool) -> DL -> DL
addsnake cd dl
    | cd pi pj = addsnake cd $
                 dl {poi = pi + 1, poj = pj + 1, path = path dl}
    | otherwise   = dl
    where pi = poi dl; pj = poj dl

-- | Compute the minimum sequence of 'DI' edit steps that transforms @as@ into
-- @bs@, returned in reverse order. The result is in direct correspondence with
-- the SES: its subsequence of /match points/ is the Longest Common Subsequence
-- (LCS).
--
-- @lcs eq as bs@ runs the Myers O(ND) BFS algorithm following
-- a five-step pipeline:
--
-- 1. __Seed__: create the initial single-node frontier @[addsnake cd (DL 0 0 [])]@
--    corresponding to the upper bound of the longest origin-sourced snake.
-- 2. __Iterate__: apply 'dstep' repeatedly via 'iterate', producing an
--    infinite list of frontiers (one per edit distance D = 0, 1, 2, …).
-- 3. __Flatten__: 'concat' all frontiers into a single stream of 'DL' nodes.
-- 4. __Find__: 'dropWhile' skips nodes until one reaches @(lena, lenb)@ — the
--    bottom-right corner of the edit graph — which is the terminal node of a
--    shortest edit script.
-- 5. __Extract__: 'head' returns that node; its 'path' field carries the edit
--    trace in reverse order.
--
-- This implementation is purely functional: rather than updating a shared
-- frontier array in place, as in the original paper, it builds a new list of
-- 'DL' nodes for each value of \( D \) and concatenates them into a single
-- lazy stream. This is simpler but carries a larger per-node overhead: each
-- 'DL' holds its own edit trace as a @['DI']@ list that structurally shares
-- its tail with the parent node's trace (consing one step reuses the
-- existing spine), rather than the paper's single-integer-per-diagonal
-- representation. The asymptotic time
-- and space complexity — \( O(ND) \) and \( O(D^2) \) respectively — is
-- unchanged. Unlike the paper, which selects the better candidate per
-- diagonal before extending its snake, 'dstep' extends snakes on /both/
-- candidates before 'pairMaxes' selects the winner, discarding the other
-- extension. This does not affect the time bound: on any given diagonal,
-- all snake intervals — retained and discarded — are non-overlapping across
-- successive values of \( D \), because each new candidate starts at or
-- beyond the previous winner's endpoint. The total number of element
-- comparisons across all snake extensions is therefore \( O(ND) \).
lcs :: (a -> b -> Bool) -> [a] -> [b] -> [DI]
lcs eq as bs = path . head . dropWhile (\dl -> poi dl /= lena || poj dl /= lenb) .
            concat . iterate (dstep cd) . (:[]) . addsnake cd $
            DL {poi=0,poj=0,path=[]}
            where cd = canDiag eq as bs lena lenb
                  lena = length as; lenb = length bs

-- | Takes two lists and returns a list of differences between them. This is
-- 'getDiffBy' with '==' used as predicate.
--
-- > > getDiff ["a","b","c","d","e"] ["a","c","d","f"]
-- > [Both "a" "a",First "b",Both "c" "c",Both "d" "d",First "e",Second "f"]
-- > > getDiff "abcde" "acdf"
-- > [Both 'a' 'a',First 'b',Both 'c' 'c',Both 'd' 'd',First 'e',Second 'f']
getDiff :: (Eq a) => [a] -> [a] -> [Diff a]
getDiff = getDiffBy (==)

-- | Takes two lists and returns a list of differences between them, grouped
-- into chunks. This is 'getGroupedDiffBy' with '==' used as predicate.
--
-- > > getGroupedDiff "abcde" "acdf"
-- > [Both "a" "a",First "b",Both "cd" "cd",First "e",Second "f"]
getGroupedDiff :: (Eq a) => [a] -> [a] -> [Diff [a]]
getGroupedDiff = getGroupedDiffBy (==)

-- | A form of 'getDiff' with no 'Eq' constraint. Instead, an equality predicate
-- is taken as the first argument.
getDiffBy :: (a -> b -> Bool) -> [a] -> [b] -> [PolyDiff a b]
getDiffBy eq a b = markup a b . reverse $ lcs eq a b
    where markup (x:xs) (y:ys) ds
            | eq x y = Both x y : markup xs ys ds
          markup (x:xs)   ys   (F:ds) = First x  : markup xs ys ds
          markup   xs   (y:ys) (S:ds) = Second y : markup xs ys ds
          markup _ _ _ = []

-- | Like 'getGroupedDiff' but accepts a custom equality predicate.
getGroupedDiffBy :: (a -> b -> Bool) -> [a] -> [b] -> [PolyDiff [a] [b]]
getGroupedDiffBy eq a b = go $ getDiffBy eq a b
    where go (First x  : xs) = let (fs, rest) = goFirsts  xs in First  (x:fs)     : go rest
          go (Second x : xs) = let (fs, rest) = goSeconds xs in Second (x:fs)     : go rest
          go (Both x y : xs) = let (fs, rest) = goBoth    xs
                                   (fxs, fys) = unzip fs
                               in Both (x:fxs) (y:fys) : go rest
          go [] = []

          goFirsts  (First x  : xs) = let (fs, rest) = goFirsts  xs in (x:fs, rest)
          goFirsts  xs = ([],xs)

          goSeconds (Second x : xs) = let (fs, rest) = goSeconds xs in (x:fs, rest)
          goSeconds xs = ([],xs)

          goBoth    (Both x y : xs) = let (fs, rest) = goBoth xs    in ((x,y):fs, rest)
          goBoth    xs = ([],xs)
