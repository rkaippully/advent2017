module Advent.Day10 (day10part1, day10part2) where

import           Advent.KnotHash (simpleHash, knotHash, hashToHex)
import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString, fromByteString)
import qualified Data.ByteString.Char8 as BS


day10part1 :: Problem
day10part1 = Problem "day10part1" $ \s ->
  let
    [[n], lengths] = (map fromByteString . BS.split ',') <$> BS.lines s
  in
    toByteString $ simpleHash n lengths

day10part2 :: Problem
day10part2 = Problem "day10part2" $ \s ->
  let
    [n, str] = BS.lines s
  in
    hashToHex $ knotHash (fromByteString n) str
