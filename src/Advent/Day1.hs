module Advent.Day1 (day1part1, day1part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char (digitToInt)
import           Data.Int (Int64)

solve :: (Int64 -> Int64) -> BS.ByteString -> BS.ByteString
solve dropper s =
  let
    l = fromIntegral $ BS.length s
    s' = BSL.take l $ BSL.drop (dropper l) $ BSL.cycle $ BSL.fromStrict s
  in
    toByteString $ sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ BSL.zip (BSL.fromStrict s) s'

day1part1 :: Problem
day1part1 = Problem "day1part1" $ solve (const 1)

day1part2 :: Problem
day1part2 = Problem "day1part2" (solve (`div` 2))
