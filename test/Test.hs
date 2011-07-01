module Main where

import Test.QuickCheck

import Data.Algorithm.Diff

main = subTests >> diffTests

-- We need some quick and dirty subsequence stuff for the diff tests,
-- so we build that and some tests for it.

-- | Determines whether one list is a subsequence of another.
isSub :: (Eq a) => [a] -> [a] -> Bool
isSub [] _ = True
isSub (_:_) [] = False
isSub (x:xs) (y:ys) | x == y = isSub xs ys
                    | otherwise = isSub (x:xs) ys

-- | Lists the subsequences of a list.
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:rest) = map (x:) restss ++ restss
  where restss = subs rest

subTests :: IO ()
subTests = do
  mapM_ (quickCheck . forAll shortLists) ([
    prop_emptyInSubs,
    prop_selfInSubs,
    prop_countSubs,
    prop_everySubIsSub] :: [[Bool] -> Bool])
  quickCheck $ forAll2 shortLists (prop_sub :: [Bool] -> [Bool] -> Bool)


prop_emptyInSubs = elem [] . subs
prop_selfInSubs xs = elem xs (subs xs)
prop_countSubs xs = length (subs xs) == 2^(length xs)
prop_sub xs ys = isSub xs ys == elem xs (subs ys)
prop_everySubIsSub xs = all (flip isSub xs) (subs xs)


-- | Obtains a longest common subsequence of two lists using their
-- diff. Note that there is an @lcs@ function in the
-- 'Data.Algorithm.Diff' module, but it's not exported. It's trivial
-- to reconstruct the LCS though, just by taking the 'B' elements.
diffLCS :: (Eq a) => [a] -> [a] -> [a]
diffLCS xs ys = recoverLCS $ getDiff xs ys

-- | Recovers the (longest) common subsequence from a diff.
recoverLCS :: [(DI, a)] -> [a]
recoverLCS = filterByDI (B==)

-- | Recovers the first list from a diff.
recoverFirst :: [(DI, a)] -> [a]
recoverFirst = filterByDI (not . (S==))

-- | Recovers the second list from a diff.
recoverSecond :: [(DI, a)] -> [a]
recoverSecond = filterByDI (not . (F==)) 

filterByDI :: (DI -> Bool) -> [(DI, a)] -> [a]
filterByDI pred = map snd . filter (pred . fst)

-- | Indicates whether a list is a longest common subsequence of two
-- lists.
isLCS :: (Eq a) => [a] -> [a] -> [a] -> Bool
isLCS ss xs ys = isSub ss ys && isSub ss ys && length ss == lenLCS xs ys

-- | Computes the length of the longest common subsequence of two
-- lists. This is a naive and inefficient recursive implementation
-- that doesn't memoize repeated sub-calls, so don't use it with large
-- lists.
lenLCS :: (Eq a) => [a] -> [a] -> Int
lenLCS [] _ = 0
lenLCS _ [] = 0
lenLCS (x:xs) (y:ys) | x == y = 1 + lenLCS xs ys
                     | otherwise = max (lenLCS (x:xs) ys) (lenLCS xs (y:ys))

diffTests = do
  mapM_ (quickCheck . forAll2 shortLists) ([
    prop_recoverFirst,
    prop_recoverSecond,
    prop_lcs ] :: [[Bool] -> [Bool] -> Bool])
  mapM_ (quickCheck. forAll shortLists) ([
    prop_lcsEmpty,
    prop_lcsSelf ] :: [[Bool] -> Bool])

prop_recoverFirst xs ys = recoverFirst (getDiff xs ys) == xs
prop_recoverSecond xs ys = recoverSecond (getDiff xs ys) == ys
prop_lcs xs ys = isLCS (diffLCS xs ys) xs ys
prop_lcsEmpty xs = diffLCS xs [] == [] && diffLCS [] xs == []
prop_lcsSelf xs = diffLCS xs xs == xs


-- | Lists of no more than ten elements.
shortLists :: (Arbitrary a) => Gen [a]
shortLists = sized $ \n -> resize (min n 10) $ listOf arbitrary

-- | 'forAll' where the generator is used twice.
forAll2 :: (Show a, Testable prop) => Gen a -> (a -> a -> prop) -> Property
forAll2 gen f = forAll gen $ \x -> forAll gen (f x)