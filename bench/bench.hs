module Main where

import Criterion.Main
import Control.DeepSeq
import System.Random

import Data.Algorithm.Diff

instance NFData DI

main :: IO ()
main = do
  g <- newStdGen
  let rbools = randoms g :: [Bool]
  let (s1000_1, rbools1) = splitAt 1000 rbools
  let (s1000_2, rbools2) = splitAt 1000 rbools1
  s1000_1 `deepseq` s1000_2 `deepseq` defaultMain [
    bgroup "diff bool lists" $ [bench "1000 bools" $ nf (getDiff s1000_1) s1000_2]
    ]
