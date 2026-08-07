{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Criterion.Main
import Control.DeepSeq
import GHC.Generics
import System.Random

import Data.Algorithm.Diff

deriving instance Generic (Diff a)

instance NFData a => NFData (Diff a)

main :: IO ()
main = doBenchMarks 37

doBenchMarks :: Int -> IO ()
doBenchMarks seed =
  let rbools = randoms (mkStdGen seed) :: [Bool]
      (s1000_1, rbools1) = splitAt 1000 rbools
      (s1000_2, rbools2) = splitAt 1000 rbools1
      s500_2 = take 500 s1000_2
  in (s1000_1, s1000_2, s500_2) `deepseq` defaultMain
      [ bgroup "diff bool lists"
          [ bench "1000 bools" $ nf (getDiff s1000_1) s1000_2
          , bench "1000/500 bools" $ nf (getDiff s1000_1) s500_2
          ]
      ]
