-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithm.DiffOutput
-- Copyright   :  (c) Sterling Clover 2008-2011, Kevin Charter 2011
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- Author      :  Stephan Wehr (wehr@factisresearch.com) and JP Moresmau (jp@moresmau.fr)
--
-- Generates a string output that is similar to diff normal mode
-----------------------------------------------------------------------------
module Data.Algorithm.DiffOutput where

import Data.Algorithm.Diff

import Text.PrettyPrint

-- | pretty print the differences. The output is similar to the output of the diff utility
ppDiff :: [Diff [String]] -> String
ppDiff gdiff =
   let  diffLineRanges = toLineRange 1 1 gdiff
   in   
        render (prettyDiffs diffLineRanges) ++ "\n"
   where
          toLineRange :: Int -> Int -> [Diff [String]] -> [DiffOperation LineRange]
          toLineRange _ _ []=[]
          toLineRange leftLine rightLine (Both ls _:rs)=
                let lins=length ls
                in toLineRange (leftLine+lins) (rightLine+lins) rs
          toLineRange leftLine rightLine (Second lsS:First lsF:rs)=
                toChange leftLine rightLine lsF lsS rs
          toLineRange leftLine rightLine (First lsF:Second lsS:rs)=
                toChange leftLine rightLine lsF lsS rs
          toLineRange leftLine rightLine (Second lsS:rs)=
                let linesS=length lsS
                    diff=Addition (LineRange (rightLine,rightLine+linesS-1) lsS) (leftLine-1)
                in diff : toLineRange leftLine (rightLine+linesS) rs      
          toLineRange leftLine rightLine  (First lsF:rs)=
                let linesF=length lsF
                    diff=Deletion (LineRange (leftLine,leftLine+linesF-1) lsF) (rightLine-1)
                in diff: toLineRange(leftLine+linesF) rightLine rs
          toChange leftLine rightLine lsF lsS rs=
                let linesS=length lsS
                    linesF=length lsF
                in Change (LineRange (leftLine,leftLine+linesF-1) lsF) (LineRange (rightLine,rightLine+linesS-1) lsS)
                        : toLineRange (leftLine+linesF) (rightLine+linesS) rs
                
-- | pretty print of diff operations                
prettyDiffs :: [DiffOperation LineRange] -> Doc
prettyDiffs [] = empty
prettyDiffs (d : rest) = prettyDiff d $$ prettyDiffs rest
    where
      prettyDiff (Deletion inLeft lineNoRight) =
          prettyRange (lrNumbers inLeft) <> char 'd' <> int lineNoRight $$
          prettyLines '<' (lrContents inLeft)
      prettyDiff (Addition inRight lineNoLeft) =
          int lineNoLeft <> char 'a' <> prettyRange (lrNumbers inRight) $$
          prettyLines '>' (lrContents inRight)
      prettyDiff (Change inLeft inRight) =
          prettyRange (lrNumbers inLeft) <> char 'c' <> prettyRange (lrNumbers inRight) $$
          prettyLines '<' (lrContents inLeft) $$
          text "---" $$
          prettyLines '>' (lrContents inRight)
      prettyRange (start, end) =
          if start == end then int start else int start <> comma <> int end
      prettyLines start lins =
          vcat (map (\l -> char start <+> text l) lins)                
                
type LineNo = Int

data LineRange = LineRange { lrNumbers :: (LineNo, LineNo)
                           , lrContents :: [String]
                           }
            deriving (Show)

data DiffOperation a = Deletion a LineNo
            | Addition a LineNo
            | Change a a
            deriving (Show)                