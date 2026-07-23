-- | Utilify functions lifted into Liquid Haskell logic.
--
-- The /raison d'être/ for this module is that functions need to be explicitly
-- /lifted/ into the LiquidHaskell logic before we can call them in refinement
-- predicates.
-- In general, we need access to a functions unfoldings for the relevant constraints
-- to be produced. At the time of writing, GHC does not reliable exposes dependency
-- unfoldings in interface files; so the most robust work around is
-- to reimplement certain functions locally.
-- For a detailed discussion see:
-- <https://www.tweag.io/blog/2024-09-12-lh-reflection/>
module Internal.LiftedFunctions where
 
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
