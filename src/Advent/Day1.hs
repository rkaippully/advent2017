module Advent.Day1 where

import           Advent.Types (Problem(Problem))
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char (digitToInt)
import           Data.Int (Int64)

solve :: (Int64 -> Int64) -> BS.ByteString -> BS.ByteString
solve dropper s =
  let
    l = BS.length s
    s' = BS.take l $ BS.drop (dropper l) $ BS.cycle s
  in
    BS.pack $ show $ sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ BS.zip s s'

day1part1 :: Problem
day1part1 = Problem "day1part1" $ solve (const 1)

day1part2 :: Problem
day1part2 = Problem "day1part2" (solve (`div` 2))
