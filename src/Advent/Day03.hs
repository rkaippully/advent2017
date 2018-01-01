module Advent.Day03 (day03part1, day03part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString, fromByteString)

data Side = SRight | STop | SLeft | SBottom
  deriving (Eq)

type Coord = (Int, Int)

coordinates :: [Coord]
coordinates = (0, 0) : gen SRight (1, 0) 1
  where
    gen :: Side -> Coord -> Int -> [Coord]
    gen SRight v@(x, y) lvl  | y == lvl  = v : gen STop (x - 1, y) lvl
                             | otherwise = v : gen SRight (x, y + 1) lvl
    gen STop v@(x, y) lvl    | x == negate lvl = v : gen SLeft (x, y - 1) lvl
                             | otherwise       = v : gen STop (x - 1, y) lvl
    gen SLeft v@(x, y) lvl   | y == negate lvl = v : gen SBottom (x + 1, y) lvl
                             | otherwise       = v : gen SLeft (x, y - 1) lvl
    gen SBottom v@(x, y) lvl | x == lvl  = v : gen SRight (x + 1, y) (lvl + 1)
                             | otherwise = v : gen SBottom (x + 1, y) lvl

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

day03part1 :: Problem
day03part1 = Problem "day03part1" $ \s ->
  let
    n = fromByteString s
  in
    toByteString $ manhattanDistance (coordinates!!(n - 1)) (0, 0)

sums :: [Int]
sums = 1 : map sumAt [1..]
  where
    sumAt :: Int -> Int
    sumAt idx = sum $ map snd $ filter (isAdjacent (coordinates!!idx)) $ zip (take idx coordinates) sums

    isAdjacent :: Coord -> (Coord, a) -> Bool
    isAdjacent (x1, y1) ((x2, y2), _) = abs (x2 - x1) <= 1 && abs (y2 - y1) <= 1

day03part2 :: Problem
day03part2 = Problem "day03part2" $ \s ->
  let
    n = fromByteString s
  in
    toByteString $ head $ dropWhile (<= n) sums
