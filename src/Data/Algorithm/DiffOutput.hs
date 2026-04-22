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
-- Generates a string output that is similar to diff normal mode.
-----------------------------------------------------------------------------
module Data.Algorithm.DiffOutput where
import Data.Algorithm.Diff
import Text.PrettyPrint hiding ((<>))
import Data.Char
import Data.List

-- | Converts 'Diff's to 'DiffOperation's. 'First' and 'Second'
-- ocurrances are converted to 'Addition' and 'Deletion', respectively, while
-- consecutive ocurrances of them are replaced by a 'Change'.
diffToLineRanges :: [Diff [String]] -> [DiffOperation LineRange]
diffToLineRanges = toLineRange 1 1
   where
          -- | In @toLineRange x y ds@, @x@ is the index of the current string in the
          -- left input of the diff @ds@, and @y@ is the index of the corresponding
          -- string in the right input of the diff @ds@.
          toLineRange :: Int -> Int -> [Diff [String]] -> [DiffOperation LineRange]
          toLineRange _ _ []=[]
          -- If the lines are the same, we just move forward.
          toLineRange leftLine rightLine (Both ls _:rs)=
                let lins=length ls
                in  toLineRange (leftLine+lins) (rightLine+lins) rs
          -- A 'Change' is introduced when an addition is followed by a deletion, or vice versa.
          toLineRange leftLine rightLine (Second lsS:First lsF:rs)=
                toChange leftLine rightLine lsF lsS rs
          toLineRange leftLine rightLine (First lsF:Second lsS:rs)=
                toChange leftLine rightLine lsF lsS rs
          -- Introduce 'Addition's.
          toLineRange leftLine rightLine (Second lsS:rs)=
                let linesS=length lsS
                    diff=Addition (LineRange (rightLine,rightLine+linesS-1) lsS) (leftLine-1)
                in  diff : toLineRange leftLine (rightLine+linesS) rs
          -- Introduce 'Deletion's.
          toLineRange leftLine rightLine  (First lsF:rs)=
                let linesF=length lsF
                    diff=Deletion (LineRange (leftLine,leftLine+linesF-1) lsF) (rightLine-1)
                in  diff: toLineRange(leftLine+linesF) rightLine rs
          -- | Build 'Change's from adjacent additions and deletions.
          toChange :: Int -- ^ Current left line number.
                   -> Int -- ^ Current right line number.
                   -> [String] -- ^ Lines from the 'First' list (corresponding to deletions).
                   -> [String] -- ^ Lines from the 'Second' list (corresponding to additions).
                   -> [Diff [String]] -- ^ Remaining 'Diff's.
                   -> [DiffOperation LineRange]
          toChange leftLine rightLine lsF lsS rs=
                let linesS=length lsS
                    linesF=length lsF
                in  Change (LineRange (leftLine,leftLine+linesF-1) lsF) (LineRange (rightLine,rightLine+linesS-1) lsS)
                        : toLineRange (leftLine+linesF) (rightLine+linesS) rs

-- | Pretty print the differences. The output is similar to the output of the @diff@ utility.
--
-- > > putStr (ppDiff (getGroupedDiff ["a","b","c","d","e"] ["a","c","d","f"]))
-- > 2d1
-- > < b
-- > 5c4
-- > < e
-- > ---
-- > > f
ppDiff :: [Diff [String]] -> String
ppDiff gdiff =
   let  diffLineRanges = diffToLineRanges gdiff
   in
        render (prettyDiffs diffLineRanges) ++ "\n"


-- | Pretty print of diff operations.
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

-- | Parse pretty printed 'Diff's as 'DiffOperation's.
parsePrettyDiffs :: String -> [DiffOperation LineRange]
parsePrettyDiffs = reverse . doParse [] . lines
  where
    -- | Parsing entry point that iteratively accumulates 'DiffOperation's
    -- until the input is exhausted.
    doParse :: [DiffOperation LineRange] -> [String] -> [DiffOperation LineRange]
    -- NOTE: Incorrectly formatted lines are ignored.
    doParse acc [] = acc
    doParse acc s =
        let (mnd,r) = parseDiff s
        in case mnd of
            Just nd -> doParse (nd:acc) r
            _          -> doParse acc r

    parseDiff :: [String] -> (Maybe (DiffOperation LineRange), [String])
    parseDiff [] = (Nothing,[])
    parseDiff (h:rs) = let
        (r1,hrs1) = parseRange h
        in case hrs1 of
                -- In each case, we pass the left line range,
                -- the remaining string after the type character,
                -- which must contain the right line range,
                -- and the remaining lines to parse.
                ('d':hrs2) -> parseDel r1 hrs2 rs
                ('a':hrs2) -> parseAdd r1 hrs2 rs
                ('c':hrs2) -> parseChange r1 hrs2 rs
                _ -> (Nothing,rs)

    parseDel :: (LineNo, LineNo) -> String -> [String] -> (Maybe (DiffOperation LineRange), [String])
    parseDel r1 hrs2 rs = let
        -- NOTE: the wildcard should correspond to the end of line,
        -- but is ignored for simplicity.
        (r2,_) = parseRange hrs2
        (ls,rs2) = span (isPrefixOf "<") rs
        in (Just $ Deletion (LineRange r1 (map (drop 2) ls)) (fst r2), rs2)

    parseAdd :: (LineNo, LineNo) -> String -> [String] -> (Maybe (DiffOperation LineRange), [String])
    parseAdd r1 hrs2 rs = let
        -- NOTE: the wildcard should correspond to the end of line,
        -- but is ignored for simplicity.
        (r2,_) = parseRange hrs2
        (ls,rs2) = span (isPrefixOf ">") rs
        in (Just $ Addition (LineRange r2 (map (drop 2) ls)) (fst r1), rs2)

    parseChange :: (LineNo, LineNo) -> String -> [String] -> (Maybe (DiffOperation LineRange), [String])
    parseChange r1 hrs2 rs = let
        -- NOTE: the wildcard should correspond to the end of line,
        -- but is ignored for simplicity.
        (r2,_) = parseRange hrs2
        (ls1,rs2) = span (isPrefixOf "<") rs
        in case rs2 of
            -- The left and right diff of a 'Change' are separated by a "---" line.
            ("---":rs3) -> let
                (ls2,rs4) = span (isPrefixOf ">") rs3
                in (Just $ Change (LineRange r1 (map (drop 2) ls1)) (LineRange r2 (map (drop 2) ls2)), rs4)
            _ -> (Nothing,rs2)

    parseRange :: String -> ((LineNo, LineNo),String)
    parseRange l = let
        (fstLine,rs) = span isDigit l
        (sndLine,rs3) = case rs of
                                    -- The comma is used to separate
                                    -- the start and end line numbers in a range,
                                    -- but is omitted if they are the same.
                                    -- i.e. the range is a single line.
                                    (',':rs2) -> span isDigit rs2
                                    _ -> (fstLine,rs)
        in ((read fstLine,read sndLine),rs3)

-- | Line number alias.
type LineNo = Int

-- | Line Range: start, end and contents.
data LineRange = LineRange { lrNumbers :: (LineNo, LineNo)
                           , lrContents :: [String]
                           }
            deriving (Show, Read, Eq, Ord)

-- | Diff operation representing changes to apply.
data DiffOperation a
  = Deletion a LineNo  -- ^ Element deleted on the left input, line number
                       -- preceding the deleted lines in the right input.
  | Addition a LineNo  -- ^ Element added from the right input, line number
                       -- preceding the added lines in the left input.
  | Change a a         -- ^ Element changed from the left input to the right input.
  deriving (Show,Read,Eq,Ord)
