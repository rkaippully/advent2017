module Advent.Day6 (day6part1, day6part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString, fromByteString, rotateLeft, rotateRight)
import qualified Data.ByteString.Char8 as BS
import           Data.HashMap.Lazy (HashMap, empty, member, insertWith, (!))
import           Data.Ord (Down(Down))


type Banks = [Int]
type StepNum = Int

data StepInfo = StepInfo {
  seenBanks :: HashMap Banks StepNum
  , stepNum :: StepNum
  , stepBank :: Banks
  , isValid :: Bool}
  deriving (Show)

redistribute :: Int -> Banks -> Banks
redistribute len blks =
  let
    (n, Down idx) = maximum $ zip blks $ map Down [0..]

    -- Remove all blocks at `idx`
    blks' = 0 : tail (rotateLeft idx blks)

    (d, m) = n `divMod` len

    -- These are the sums to be added to `blks'` for redistribution
    sums :: [Int]
    sums = rotateRight 1 $ zipWith (+) (replicate len d) (replicate m 1 ++ replicate (len - m) 0)
  in
    rotateRight idx $ zipWith (+) blks' sums

checkRepeat :: StepInfo -> Banks -> StepInfo
checkRepeat info x = StepInfo {
  seenBanks = insertWith (flip const) x n seen -- keep the old StepNum on duplicates
  , stepNum = n + 1
  , stepBank = x
  , isValid = not (x `member` seen)}
  where
    n = stepNum info
    seen = seenBanks info

allocs :: Banks -> [StepInfo]
allocs blocks =
  let
    len = length blocks
    start = StepInfo {
      seenBanks = empty
      , stepNum = 0
      , stepBank = blocks
      , isValid = True}
  in
    scanl checkRepeat start $ iterate (redistribute len) blocks

day6part1 :: Problem
day6part1 = Problem "day6part1" $ \s ->
  let
    blocks = map fromByteString $ BS.words s
  in
    toByteString $ length (takeWhile isValid $ allocs blocks) - 1

day6part2 :: Problem
day6part2 = Problem "day6part2" $ \s ->
  let
    blocks = map fromByteString $ BS.words s
    invalid = head $ dropWhile isValid $ allocs blocks
  in
    toByteString $ stepNum invalid - seenBanks invalid!stepBank invalid - 1
