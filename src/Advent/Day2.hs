module Advent.Day2 where

import           Advent.Types (Problem(Problem))
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char (isSpace)

largestAndSmallest :: [Int] -> (Int, Int)
largestAndSmallest xs = foldr (\x (l, s) -> (if x > l then x else l, if x < s then x else s)) (head xs, head xs) (tail xs)

day2part1 :: Problem
day2part1 = Problem "day2part1" $ \s ->
  let
    toRow line = read . BS.unpack <$> BS.groupBy (\_ c -> isSpace c) line
    rows = map toRow (BS.lines s)
  in
    BS.pack $ show $ sum $ map (uncurry (-) . largestAndSmallest) rows

day2part2 :: Problem
day2part2 = Problem "day2part2" undefined
