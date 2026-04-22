{-@ LIQUID "--ple" @-}
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
-- * 'Both' — the element is common to both inputs.
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
-- Precondition: The node list must be non-empty.
{-@
dstep
  :: (Nat -> Nat -> Bool)
  -> d : Nat
  -> {nodes : WaveFront d | len nodes > 0}
  -> {v : WaveFront (d + 1) | len v = len nodes + 1}
@-}
dstep
  :: (Int -> Int -> Bool) -- ^ Diagonal predicate
  -> Int                  -- ^ The current D-length; used for the static check of wave front invariant.
  -> [DL]                 -- ^ A non-empty wave front of nodes at edit distance D
  -> [DL]                 -- ^ A non-empty wave front of nodes at edit distance D+1
dstep _ _d [] = error "dstep: Cannot perform expansion on an empty list of nodes"
dstep cd _ (dl:dls) = addsnake cd (hStep dl) : stepAndMerge dl dls
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
    stepAndMerge :: DL -> [DL] -> [DL]
    stepAndMerge prev [] = [addsnake cd $ vStep prev]
    stepAndMerge prev (next:rest) =
      addsnake cd (furthestReaching (vStep prev) (hStep next)) : stepAndMerge next rest

{-@ lazy addsnake @-}
-- | Follow a /snake/ from the current position of a 'DL' node.
--
-- A snake is a sequence of diagonal (cost-free) edges in the edit graph,
-- i.e. a run of equal elements that can be consumed simultaneously
-- from both input sequences without counting as an edit.  Starting from
-- @(poi dl, poj dl)@, this function advances both 'poi' and 'poj' as long
-- as consecutive elements match, leaving 'path' unchanged (diagonal moves
-- are not recorded as edit steps).
{-@ addsnake :: (Nat -> Nat -> Bool) -> x : DL -> {v : DL | path v == path x && _kdiag v = _kdiag x} @-}
addsnake :: (Int -> Int -> Bool) -> DL -> DL
addsnake cd dl
    | cd pi pj = addsnake cd $
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
            concat . iterate (uncurry (dstep cd) . withD) . (:[]) . addsnake cd $
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
