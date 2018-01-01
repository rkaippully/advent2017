module Advent.Day02 (day02part1, day02part2) where

import           Advent.Types (Problem(Problem))
import qualified Data.ByteString.Char8 as BS
import           Data.List (tails, sort)
import Advent.Util (toByteString, fromByteString)

solve :: ([Int] -> Int) -> BS.ByteString -> BS.ByteString
solve f s =
  let
    toRow line = fromByteString <$> BS.words line
    rows = map toRow (BS.lines s)
  in
    toByteString $ sum $ map f rows

day02part1 :: Problem
day02part1 = Problem "day02part1" $ solve (\xs -> maximum xs - minimum xs)

day02part2 :: Problem
day02part2 = Problem "day02part2" $ solve evenlyDivisible
  where
    evenlyDivisible :: [Int] -> Int
    evenlyDivisible xs = head [d | (y:ys) <- tails (sort xs), (d, 0) <- (`divMod` y) <$> ys]
