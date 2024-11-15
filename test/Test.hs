{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Data.Algorithm.Diff
import Data.Algorithm.DiffContext
import Data.Algorithm.DiffOutput
import qualified Data.Array as A
import Data.Foldable
import Data.Semigroup (Arg(..))
import Text.PrettyPrint

import System.IO
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import System.Environment (getArgs)
import Data.Maybe (mapMaybe, catMaybes)
import System.Process (readProcessWithExitCode)
import System.Directory (getTemporaryDirectory)


main :: IO ()
main = defaultMain [ testGroup "sub props" [
                        slTest "empty in subs" prop_emptyInSubs,
                        slTest "self in subs"  prop_selfInSubs,
                        slTest "count subs"    prop_countSubs,
                        slTest "every sub is a sub" prop_everySubIsSub,
                        slTest2 "sub prop" prop_sub
                     ],
                     testGroup "diff props" [
                        slTest "lcsEmpty" prop_lcsEmpty,
                        slTest "lcsSelf" prop_lcsSelf,
                        slTest2 "lcsBoth" prop_lcsBoth,
                        slTest2 "recover first" prop_recoverFirst,
                        slTest2 "recover second" prop_recoverSecond,
                        slTest2 "lcs" prop_lcs,
                        testProperty "compare random with reference" prop_compare_with_reference
                     ],
                     testGroup "output props" [
                        testProperty "self generates empty" $ forAll shortLists  prop_ppDiffEqual,
                        --testProperty "compare our lists with diff" $ forAll2 shortLists  prop_ppDiffShort,
                        testProperty "compare random with diff" prop_ppDiffR,
                        testProperty "compare with diff, issue #5" $ prop_ppDiffR
                          (DiffInput
                            { diLeft = ["1","2","3","4","","5","6","7"]
                            , diRight = ["1","2","3","q","b","u","l","","XXX6",""]
                            }),
                        testProperty "test parse" prop_parse
                     ],
                     testGroup "context props" [
                        testProperty "test context" $ prop_ppContextDiffUnitTest
                          (DiffInput
                            { diLeft = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"]
                            , diRight = ["a", "b", "d", "e", "f", "g", "h", "i", "j"]
                            })
                          "--- a\n+++ b\n@@ -1,5 +1,4 @@\n a\n b\n-c\n d\n e\n@@ -9,3 +8,2 @@\n i\n j\n-k\n",
                        testProperty "compare with empty" $ prop_ppContextDiffUnitTest
                          (DiffInput
                            { diLeft = []
                            , diRight = ["1","2","3"]
                            })
                          "--- a\n+++ b\n@@ --0,0 +1,3 @@\n+1\n+2\n+3\n",
                        testProperty "compare with empty" $ prop_ppContextDiffUnitTest
                          (DiffInput
                            { diLeft = ["1","2","3"]
                            , diRight = []
                            })
                          "--- a\n+++ b\n@@ -1,3 +-0,0 @@\n-1\n-2\n-3\n"
                     ]
                   ]
slTest s t = testProperty s $ forAll shortLists   (t :: [Bool] -> Bool)
slTest2 s t = testProperty s $ forAll2 shortLists (t :: [Bool] -> [Bool] -> Bool)

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
recoverLCS :: [Diff a] -> [a]
recoverLCS (Both x _ : xs) = x : recoverLCS xs
recoverLCS (_ : xs) = recoverLCS xs
recoverLCS [] = []

-- | Recovers the first list from a diff.
recoverFirst :: [Diff a] -> [a]
recoverFirst (First x  : xs) = x : recoverFirst xs
recoverFirst (Both x _ : xs) = x : recoverFirst xs
recoverFirst (_ : xs) = recoverFirst xs
recoverFirst [] = []

-- | Recovers the second list from a diff.
recoverSecond :: [Diff a] -> [a]
recoverSecond (Second x  : xs) = x : recoverSecond xs
recoverSecond (Both x _ : xs) = x : recoverSecond xs
recoverSecond (_ : xs) = recoverSecond xs
recoverSecond [] = []

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


prop_recoverFirst xs ys = recoverFirst (getDiff xs ys) == xs
prop_recoverSecond xs ys = recoverSecond (getDiff xs ys) == ys
prop_lcs xs ys = isLCS (diffLCS xs ys) xs ys
prop_lcsEmpty xs = diffLCS xs [] == [] && diffLCS [] xs == []
prop_lcsSelf xs = diffLCS xs xs == xs
prop_lcsBoth xs ys = all areMatch $ getDiff xs ys
    where areMatch (Both x y) = x == y
          areMatch _ = True

-- | Lists of no more than twelve elements.
shortLists :: (Arbitrary a) => Gen [a]
shortLists = sized $ \n -> resize (min n 12) $ listOf arbitrary

-- | 'forAll' where the generator is used twice.
forAll2 :: (Show a, Testable prop) => Gen a -> (a -> a -> prop) -> Property
forAll2 gen f = forAll gen $ \x -> forAll gen (f x)

prop_ppDiffEqual xs=ppDiff (getGroupedDiff xs xs)=="\n"

-- | truly random tests
prop_ppDiffR :: DiffInput -> Property
prop_ppDiffR (DiffInput le ri) =
    let haskDiff=ppDiff $ getGroupedDiff le ri
        utilDiff= unsafePerformIO (runDiff (unlines le) (unlines ri))
    in  cover 90 (haskDiff == utilDiff) "exact match" $
                classify (haskDiff == utilDiff) "exact match"
                        (div ((length (lines haskDiff))*100) (length (lines utilDiff)) < 110) -- less than 10% bigger
    where
      runDiff left right =
          do leftFile <- writeTemp left
             rightFile <- writeTemp right
             (ecode, out, err) <-
                 readProcessWithExitCode "diff" [leftFile, rightFile] ""
             -- putStrLn ("OUT:\n" ++ out)
             -- putStrLn ("ERR:\n" ++ err)
             -- putStrLn ("ECODE:\n" ++ show ecode)
             case ecode of
               ExitSuccess -> return out
               ExitFailure 1 -> return out
               ExitFailure i -> error ("'diff " ++ leftFile ++ " " ++ rightFile ++
                                       "' failed with exit code " ++ show i ++
                                       ": " ++ show err)
      writeTemp s =
          do dir <- getTemporaryDirectory
             (fp, h) <- openTempFile dir "HTF-diff.txt"
             hPutStr h s
             hClose h
             return fp

prop_ppContextDiffUnitTest :: DiffInput -> String -> Property
prop_ppContextDiffUnitTest (DiffInput le ri) expected =
  show diff === expected
  where
    hunks = getContextDiff (Just 2) le ri
    diff = prettyContextDiff (text "a") (text "b") (text . unnumber) hunks

-- | Check pretty printed DiffOperations can be parsed again
prop_parse :: DiffInput -> Bool
prop_parse (DiffInput le ri) =
    let difflrs = diffToLineRanges $ getGroupedDiff le ri
        output = render (prettyDiffs difflrs) ++ "\n"
        parsed = parsePrettyDiffs output
    in difflrs == parsed

data DiffInput = DiffInput { diLeft :: [String], diRight :: [String] }
               deriving (Show)

leftDiffInput = ["1", "2", "3", "4", "", "5", "6", "7"]

instance Arbitrary DiffInput where
    arbitrary =
        do let leftLines = leftDiffInput
           rightLinesLines <- mapM modifyLine (leftLines ++ [""])
           return $ DiffInput leftLines
                              (concat rightLinesLines)
      where
        randomString =
            do c <- elements ['a' .. 'z']
               return [c]
        modifyLine :: String -> Gen [String]
        modifyLine str =
            do prefixLen <- frequency [(20-i, return i) | i <- [0..5]]
               prefix <- mapM (const randomString) [1..prefixLen]
               frequency [ (5, return (prefix ++ [str]))
                         , (3, return (prefix ++ ["XXX" ++ str]))
                         , (2, return prefix)
                         , (2, return [str])]

-- | Reference implementation, very slow.
naiveGetDiffBy :: forall a b. (a -> b -> Bool) -> [a] -> [b] -> [PolyDiff a b]
naiveGetDiffBy eq as bs = reverse $ (\(Arg _ ds) -> ds) $ tbl A.! (length us, length vs)
  where
    us = A.listArray (0, length as - 1) as
    vs = A.listArray (0, length bs - 1) bs

    -- Indices run up to length us/vs *inclusive*
    tbl :: A.Array (Int, Int) (Arg Word [PolyDiff a b])
    tbl = A.listArray ((0, 0), (length us, length vs))
      [ gen ui vi | ui <- [0..length us], vi <- [0..length vs] ]

    gen :: Int -> Int -> Arg Word [PolyDiff a b]
    gen ui vi
      | ui == 0, vi == 0 = Arg 0 []
      | ui == 0
      = left'
      | vi == 0
      = top'
      | otherwise
      = if eq u v
        then min (min left' top') diag'
        else min left' top'
      where
        Arg leftL leftP = tbl A.! (ui, vi - 1)
        Arg diagL diagP = tbl A.! (ui - 1, vi - 1)
        Arg topL topP  = tbl A.! (ui - 1, vi)

        u = us A.! (ui - 1)
        v = vs A.! (vi - 1)

        left' = Arg (leftL + 1) (Second v : leftP)
        top' = Arg (topL + 1) (First u  : topP)
        diag' = Arg diagL (Both u v : diagP)

prop_compare_with_reference :: Positive Word -> [(Int, Int)] -> Property
prop_compare_with_reference (Positive x) ixs' =
  counterexample (show (as, bs, d1, d2)) $
    length (notBoth d1) === length (notBoth d2)
  where
    as = [0 .. max 100 x]
    len = length as
    ixs = filter (uncurry (/=)) $ map (\(i, j) -> (i `mod` len, j `mod` len)) $ take 100 ixs'
    bs = foldl' applySwap as ixs
    d1 = getDiffBy (==) as bs
    d2 = naiveGetDiffBy (==) as bs

    applySwap xs (i, j) = zipWith
      (\k x -> (if k == i then xs !! j else if k == j then xs !! i else x))
      [0..]
      xs

    notBoth = filter $ \case
      Both{} -> False
      _ -> True
