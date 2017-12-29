module Advent.Day11 (day11part1, day11part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString, fromByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (toUpper)
import           Data.List (foldl')


data Direction = N | NE | SE | S | SW | NW
  deriving (Eq, Show, Read)

type Cell = (Int, Int, Int)

move :: Cell -> Direction -> Cell
move (x, y, z) N  = (x, y + 1, z - 1)
move (x, y, z) NE = (x + 1, y, z - 1)
move (x, y, z) SE = (x + 1, y - 1, z)
move (x, y, z) S  = (x, y - 1, z + 1)
move (x, y, z) SW = (x - 1, y, z + 1)
move (x, y, z) NW = (x - 1, y + 1, z)

distance :: Cell -> Cell -> Int
distance (x1, y1, z1) (x2, y2, z2) = (abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)) `div` 2

day11part1 :: Problem
day11part1 = Problem "day11part1" $ \s ->
  let
    dirs = map fromByteString $ BS.map toUpper <$> BS.split ',' s
    startCell = (0, 0, 0)
    finalCell = foldl' move startCell dirs
  in
    toByteString $ distance finalCell startCell

day11part2 :: Problem
day11part2 = Problem "day11part2" undefined
