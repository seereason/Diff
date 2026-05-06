{-@ LIQUID "--ple" @-}
-- Import of the 'Data.Algorithm.Diff.Refinement' module is required for LiquidHaskell
-- specifications in this module, but is unused in the actual code.
-- The following GHC option suppresses the unused import warning.
{-# OPTIONS_GHC -Wno-unused-imports #-}
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
-- where \( M \) and \( N \) are the lengths of \( as \) and \( bs \) respectively,
-- with \( i \) increasing rightward and \( j \) increasing downward.
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
-- the number of non-diagonal moves.
--
-- Both input lists are 0-indexed, which leads to a slightly different
-- interpretation of the edit graph than in the original paper. In the paper,
-- each node represents the state of the traversal /after/ an edit, so a move
-- is the edit that /produced/ that node. Here, each node represents the state
-- /before/ an edit, so a move is the edit performed /on/ that node to yield its
-- successor. This distinction is only relevant when reading the implementation
-- alongside the paper.
--
-- === K-diagonals and the wave front
--
-- Every node \( (i,j) \) lies on the /k-diagonal/ \( k = i - j \).
-- After exactly \( D \) non-diagonal moves, every reachable node lies on one of
-- at most \( D+1 \) k-diagonals \( k \in \{-D,\,-D+2,\,\ldots,\,D-2,\,D\} \).
-- On each diagonal it suffices to track only the /furthest-reaching/ node
-- (the one with the largest \( i \)), collapsing the two-dimensional grid to a
-- one-dimensional /wave front/ indexed by \( k \).
--
-- The algorithm performs a breadth-first search over \( D = 0, 1, 2, \ldots \),
-- advancing the wave front by one edit at a time until a node reaches the goal
-- \( (M, N) \). The edit trace stored in that node is the SES, which
-- 'getDiffBy' reconstructs into a 'PolyDiff' list. The term /trace/ here
-- differs from the paper, where it denotes the sequence of k-diagonals visited
-- by the SES path; that structure is not materialised in this implementation.
-----------------------------------------------------------------------------
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
import Data.Algorithm.Diff.Type
import Data.Algorithm.Diff.Refinement (fst3, snd3, thd3, headIsFirst,
                                        headIsSecond, headIsBoth, noStuttering)

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

-- | /D-path Location/ — a node on the wave front of the Myers O(ND) diff
-- algorithm.
--
-- Each wave front consists of one 'DL' per /k-diagonal/.  A 'DL' stores the
-- endpoint coordinates and the edit trace of a \( D \)-path, i.e. a path from the
-- origin \( (0,0) \) that uses exactly \( D \) non-diagonal edges.
{-@
data DL = DL
    { poi  :: Nat
    , poj  :: Nat
    , path :: { p : [DI] | len p <= poi + poj }
    }
@-}
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

-- This refinement type alias represents a 'DL' value with a fixed /D-length/,
-- which we call a "D-path location node".
{-@ type DLN D = { x : DL | len (path x) = D } @-}

{-@ inline _kdiag @-}
-- | Computes the k-diagonal of a node.
-- Used in LiquidHaskell logic as an expression.
_kdiag :: DL -> Int
_kdiag dl = poi dl - poj dl

{-@ reflect _wfDiags @-}
{-@ _wfDiags :: Int -> xs : [DL] -> Bool / [len xs] @-}
-- | Checks if succesive nodes of a wave front lie within k-diagonals
-- differing by 2 as described in the Myers algorithm.
-- Used in LiquidHaskell logic as a predicate.
_wfDiags :: Int -> [DL] -> Bool
_wfDiags _ [] = True
_wfDiags k (dl:dls) = poi dl - poj dl == k && _wfDiags (k - 2) dls

-- A wave front is a list of 'DL' nodes, all at the same edit distance @D@,
-- with k-diagonals @D@, @D−2@, …, @-D+2@, @-D@.
{-@ type WaveFront D = {xs : [DLN D] | _wfDiags D xs} @-}

-- | Select the furthest-reaching candidate of two 'DL' nodes competing for the
-- same k-diagonal, as required by the Myers algorithm.
--
-- The candidate that has advanced further along the \( x \)-axis (larger 'poi')
-- is the furthest-reaching endpoint on that diagonal.
--
-- Precondition: arguments @x@ and @y@ in @furthestReaching x y@ are in the
-- same /k-diagonal/, meaning that
--
-- > poi x - poj x == poi y - poj y`
--
-- and both argument nodes are within the same wave front,
--
-- > length (path x) == length (path y)
{-@ furthestReaching ::  x : DL
                     -> {y : DL | _kdiag x = _kdiag y}
                     -> {v : DL | v = x || v = y} @-}
furthestReaching :: DL -> DL -> DL
furthestReaching x y
  | poi x >= poi y = x
  | otherwise      = y

-- * Proving the algorithm termination in Liquid Haskell
--
-- The original algorithm is known to terminate because a wave front /eventually/ reaches the 'endPoint'.
-- To prove this, both inputs lengths are threaded within phantom parameters throughout the implementation.
-- In essence, both lengths are used to encode the edit grid and its end point.

{-@ inline _manhattanDistance @-}
{-@ _manhattanDistance :: lena : Nat -> lenb : Nat -> {i : Nat | lena >= i} -> { j : Nat | lenb >= j} -> Nat @-}
_manhattanDistance :: Int -> Int -> Int -> Int -> Int
_manhattanDistance lena lenb i j  = lena - i + lenb - j

{-@ type DiagPred M N = i : Nat -> j : Nat -> {b : Bool | ((i >= M || j >= N) => not b)} @-}

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

{-@ ignore dstep @-}
-- | Perform one breadth-first search expansion step, advancing every wave front
-- 'DL' node by one 'DI' edit (one non-diagonal edge) and then following
-- any available snake.
--
-- For each node the 'dstep' produces two candidate successors by adding:
--
-- * An 'F' (delete) move: 'poi' incremented by 1.
-- * An 'S' (insert) move: 'poj' incremented by 1.
--
-- The resulting candidates are merged pairwise: the vertical successor of each
-- node is paired with the horizontal successor of the next node in the wave
-- front. The 'furthestReaching' between them is extended along the available
-- sequence of matching elements using 'addsnake'.
-- When this function is iterated from a single-node seed (as in 'ses'),
-- each such pair always lies on the same diagonal: an 'F' edge advances to the
-- next higher diagonal while an 'S' edge retreats to the next lower one, so the
-- two members of each pair straddle the same diagonal from opposite sides.
--
-- Precondition: The node list must be a non-empty @WaveFront@.
--
-- Postcondition: The node list must be a non-empty @WaveFront@
-- with one more node than the input.
{-@
dstep
  :: lena : Nat
  -> lenb : Nat
  -> DiagPred lena lenb
  -> d : Nat
  -> {nodes : WaveFront d | len nodes > 0}
  -> {v : WaveFront (d + 1) | len v = len nodes + 1}
@-}
dstep
  :: Int                  -- ^ First input's length phantom parameter for termination check.
  -> Int                  -- ^ Second input's length phantom parameter for termination check.
  -> (Int -> Int -> Bool) -- ^ Diagonal predicate
  -> Int                  -- ^ The current D-length; used for the static check of wave front invariant.
  -> [DL]                 -- ^ A non-empty wave front of nodes at edit distance D
  -> [DL]                 -- ^ A non-empty wave front of nodes at edit distance D+1
-- FIXME: @lena@, @lenb@ and @_d@ in the first equation are phantom (apparently unused)
-- parameters required by local LiquidHaskell specifications.
-- They sit at the first equation as a workaround to GHC removing them
-- when desugaring multi-equation definitions.
-- All could be replaced by underscores after fixing:
-- https://github.com/ucsd-progsys/liquidhaskell/issues/2704
dstep lena lenb _ _d [] = error "dstep: Cannot perform expansion on an empty list of nodes"
dstep lena lenb cd _ (dl:dls) =
  if poi dl >= lena then stepAndMerge dl dls
  else addsnake lena lenb cd (hStep dl) : stepAndMerge dl dls
  where
    {-@ hStep :: x : DLN _d -> {v : DLN (_d + 1) | _kdiag v = _kdiag x + 1} @-}
    hStep node = node {poi = poi node + 1, path = F : path node}
    {-@ vStep :: x : DLN _d -> {v : DLN (_d + 1) | _kdiag v = _kdiag x - 1} @-}
    vStep node = node {poj = poj node + 1, path = S : path node}
    -- Merge vertical step of previous node with horizontal step of next node,
    -- selecting the furthest-reaching candidate for each shared k-diagonal,
    -- and extend it along matching elements.
     {-@ stepAndMerge :: prev : DLN _d
                     -> {rest : [DLN _d] | _wfDiags (_kdiag prev - 2) rest}
                     -> {v : [DLN (_d+1)] | _wfDiags (_kdiag prev - 1) v && len v = len rest + 1}
                     / [len rest] @-}
    stepAndMerge prev nodes =
      -- When a node lying on the bottom boundary is found on the wave front
      -- all upcoming nodes are discarted because their in-bound childs would
      -- eventually need to cross the former's diagonal (in /more/ steps)
      -- to reach the endpoint, and thus are not SES candidates.
      if poj prev >= lenb then []
      else case nodes of
        [] -> [addsnake lena lenb cd $ vStep prev]
        (next:rest) ->
            -- HACK: This check saves us from an unneeded call to furthestReaching,
            -- as the horizontal child of the next node would be out-of-bounds,
            -- but in fact we could drop this child node altogether because
            -- the next node being on the right border means all previous nodes
            -- would need to cross the next node's diagonal in more steps,
            -- and thus cannot compete to the endpoint.
            -- However, this would result in a negligible performance gain
            -- and the loss of the wave front diagonal invariant,
            -- so we keep it for now.
            if poi next >= lena then addsnake lena lenb cd (vStep prev) : stepAndMerge next rest
            else addsnake lena lenb cd (furthestReaching (vStep prev) (hStep next)) : stepAndMerge next rest

-- | Follow a /snake/ from the current position of a 'DL' node.
--
-- A snake is a sequence of diagonal (cost-free) edges in the edit graph,
-- i.e. a run of equal elements that can be consumed simultaneously
-- from both input sequences without counting as an edit.  Starting from
-- @(poi dl, poj dl)@, this function advances both 'poi' and 'poj' as long
-- as consecutive elements match, leaving 'path' unchanged (diagonal moves
-- are not recorded as edit steps).
{-@
addsnake :: lena : Nat
         -> lenb : Nat
         -> DiagPred lena lenb
         -> dl : DL
         -> {v : DL | path v == path dl
                   && _kdiag v = _kdiag dl}
         / [_manhattanDistance lena lenb (poi dl) (poj dl)]
@-}
addsnake :: Int                  -- ^ First input's length phantom parameter for termination check.
         -> Int                  -- ^ Second input's length phantom parameter for termination check.
         -> (Int -> Int -> Bool) -- ^ Diagonal predicate, a.k.a. 'canDiag'
         -> DL
         -> DL
addsnake lena lenb cd dl
    | cd pi pj = addsnake lena lenb cd $
                 dl {poi = pi + 1, poj = pj + 1, path = path dl}
    | otherwise   = dl
    where pi = poi dl; pj = poj dl

{-@ ignore ses @-}
-- | Compute shortest edit script (SES), as the minimum sequence of 'DI' edit
-- steps that transforms @as@ into @bs@, returned in reverse order.
--
-- @ses eq as bs@ runs the Myers O(ND) diff algorithm following
-- a five-step pipeline:
--
-- 1. __Seed__: create an initial 0-path wave front @[addsnake cd (DL 0 0 [])]@
--    having a single node on the tip of the longest origin-sourced snake.
-- 2. __Iterate__: apply 'dstep' repeatedly via 'iterate', producing an
--    infinite list of wave fronts (one per edit distance D = 0, 1, 2, …).
-- 3. __Flatten__: 'concat' all wave fronts into a single stream of 'DL' nodes.
-- 4. __Find__: 'dropWhile' skips nodes until one reaches @(lena, lenb)@ — the
--    bottom-right corner of the edit graph — which is the terminal node of a
--    shortest edit script.
-- 5. __Extract__: 'head' returns that node; its 'path' field carries the edit
--    trace in reverse order.
--
-- This implementation is purely functional: rather than updating a shared
-- diagonal frontier array in place, as in the original paper, it builds a new
-- list of 'DL' nodes for each value of \( D \) and concatenates them into
-- a single lazy stream. This is simpler but carries a larger per-node overhead:
-- each 'DL' holds its own edit trace as a @['DI']@ list that structurally
-- shares its tail with the parent node's trace (consing one step reuses the
-- existing spine), rather than the paper's single-integer-per-diagonal
-- representation. The asymptotic time
-- and space complexity — \( O(ND) \) and \( O(D^2) \) respectively — is
-- unchanged.
ses :: (a -> b -> Bool) -> [a] -> [b] -> [DI]
ses eq as bs = path . head . dropWhile (\dl -> poi dl /= lena || poj dl /= lenb) .
            concat . iterate (uncurry (dstep lena lenb cd) . withD) . (:[]) . addsnake lena lenb cd $
            DL {poi=0,poj=0,path=[]}
            where cd = canDiag eq as bs lena lenb
                  lena = length as; lenb = length bs
                  withD xs = (length . path . head $ xs, xs)

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
{-@ getGroupedDiff :: Eq a => [a] -> [a]
                           -> {v:[GroupedDiff a a] | noStuttering v} @-}
getGroupedDiff :: (Eq a) => [a] -> [a] -> [Diff [a]]
getGroupedDiff = getGroupedDiffBy (==)

-- | A form of 'getDiff' with no 'Eq' constraint. Instead, an equality predicate
-- is taken as the first argument.
getDiffBy :: (a -> b -> Bool) -> [a] -> [b] -> [PolyDiff a b]
getDiffBy _ a [] = map First a
getDiffBy _ [] b = map Second b
getDiffBy eq a b = markup a b . reverse $ ses eq a b
    where markup (x:xs) (y:ys) ds
            | eq x y = Both x y : markup xs ys ds
          markup (x:xs)   ys   (F:ds) = First x  : markup xs ys ds
          markup   xs   (y:ys) (S:ds) = Second y : markup xs ys ds
          markup _ _ _ = []

-- | Like 'getGroupedDiff' but accepts a custom equality predicate.
--
-- Postcondition: the output list is guaranteed to be /chunked/. i.e. no two adjacent
-- elements share the same constructor.
{-@ getGroupedDiffBy :: (a -> b -> Bool) -> [a] -> [b]
                     -> {vs : [GroupedDiff a b] | noStuttering vs} @-}
getGroupedDiffBy :: (a -> b -> Bool) -> [a] -> [b] -> [PolyDiff [a] [b]]
getGroupedDiffBy eq a b = groupDiff $ getDiffBy eq a b
  where
    {-@ groupDiff :: xs : [PolyDiff a b]
                  -> {vs : [GroupedDiff a b] | noStuttering vs
                      // The following predicates allow LiquidHaskell keep track
                      // of the head constructor in each recursive call.
                      && (headIsFirst xs  <=> headIsFirst vs)
                      && (headIsSecond xs <=> headIsSecond vs)
                      && (headIsBoth xs   <=> headIsBoth vs)} @-}
    groupDiff :: [PolyDiff a b] -> [PolyDiff [a] [b]]
    groupDiff (First x  : xs) = let (fs, rest) = leadingFirsts  xs
                                 in First (x:fs) : groupDiff rest
    groupDiff (Second x : xs) = let (sc, rest) = leadingSeconds xs
                                 in Second (x:sc) : groupDiff rest
    groupDiff (Both x y : xs) = let (bxs, bys, rest) = leadingBoths xs
                                 in Both (x:bxs) (y:bys) : groupDiff rest
    groupDiff [] = []

    {-@ leadingFirsts :: xs : [PolyDiff a b]
                      -> {v : ([a], [PolyDiff a b])
                            | not (headIsFirst (snd v))
                           // Here, and in the analogous helpers,
                           // the length comparison is needed for termination check.
                           && len (snd v) <= len xs
                           && (headIsSecond xs => headIsSecond (snd v))
                           && (headIsBoth xs   => headIsBoth (snd v))} @-}
    leadingFirsts :: [PolyDiff a b] -> ([a], [PolyDiff a b])
    leadingFirsts (First y : diffs) = let (firsts, rest) = leadingFirsts diffs
                                       in (y:firsts, rest)
    leadingFirsts diffs = ([],diffs)

    {-@ leadingSeconds :: xs : [PolyDiff a b]
                       -> {v : ([b], [PolyDiff a b])
                             | not (headIsSecond (snd v))
                            && len (snd v) <= len xs
                            && (headIsFirst xs => headIsFirst (snd v))
                            && (headIsBoth xs  => headIsBoth (snd v))} @-}
    leadingSeconds :: [PolyDiff a b] -> ([b], [PolyDiff a b])
    leadingSeconds (Second y : diffs) = let (seconds, rest) = leadingSeconds diffs
                                         in (y:seconds, rest)
    leadingSeconds diffs = ([],diffs)

    {-@ leadingBoths :: xs : [PolyDiff a b]
                     -> {v : ([a], [b], [PolyDiff a b])
                           | not (headIsBoth (thd3 v))
                          && len (thd3 v) <= len xs
                          && (headIsFirst xs  => headIsFirst (thd3 v))
                          && (headIsSecond xs => headIsSecond (thd3 v))
                          && len (fst3 v) == len (snd3 v)} @-}
    leadingBoths :: [PolyDiff a b] -> ([a], [b], [PolyDiff a b])
    leadingBoths (Both w z : diffs) = let (as, bs, rest) = leadingBoths diffs
                                       in (w:as, z:bs, rest)
    leadingBoths diffs = ([], [], diffs)
