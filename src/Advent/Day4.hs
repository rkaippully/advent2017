module Advent.Day4 (day4part1, day4part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List (sort)

isValidPassphrase :: [BS.ByteString] -> Bool
isValidPassphrase (x:x1:_) | x == x1 = False
isValidPassphrase (_:xs) = isValidPassphrase xs
isValidPassphrase [] = True

day4part1 :: Problem
day4part1 = Problem "day4part1" $
  toByteString . length . filter isValidPassphrase . map (sort . BS.words) . BS.lines

day4part2 :: Problem
day4part2 = Problem "day4part2" $
  toByteString . length . filter isValidPassphrase . map (sort . map BS.sort . BS.words) . BS.lines
