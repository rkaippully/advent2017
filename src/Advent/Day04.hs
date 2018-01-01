module Advent.Day04 (day04part1, day04part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List (sort)

isValidPassphrase :: [BS.ByteString] -> Bool
isValidPassphrase (x:x1:_) | x == x1 = False
isValidPassphrase (_:xs) = isValidPassphrase xs
isValidPassphrase [] = True

day04part1 :: Problem
day04part1 = Problem "day04part1" $
  toByteString . length . filter isValidPassphrase . map (sort . BS.words) . BS.lines

day04part2 :: Problem
day04part2 = Problem "day04part2" $
  toByteString . length . filter isValidPassphrase . map (sort . map BS.sort . BS.words) . BS.lines
