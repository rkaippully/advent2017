module Advent.Day3 (day3part1, day3part2) where

import           Advent.Types (Problem(Problem))
import qualified Data.ByteString.Lazy.Char8 as BS

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

day3part1 :: Problem
day3part1 = Problem "day3part1" $ \s ->
  let
    n = read $ BS.unpack s
  in
    BS.pack $ show $ manhattanDistance (coordinates!!(n - 1)) (0, 0)

sums :: [Int]
sums = 1 : genSums 1
  where
    genSums :: Int -> [Int]
    genSums idx = sumOf idx : genSums (idx + 1)

    sumOf :: Int -> Int
    sumOf idx = sum $ map snd $ filter (isAdjacent (coordinates!!idx)) $ zip (take idx coordinates) sums

    isAdjacent :: Coord -> (Coord, a) -> Bool
    isAdjacent (x1, y1) ((x2, y2), _) = abs (x2 - x1) <= 1 && abs (y2 - y1) <= 1

day3part2 :: Problem
day3part2 = Problem "day3part2" $ \s ->
  let
    n = read $ BS.unpack s
  in
    BS.pack $ show $ head $ dropWhile (<= n) sums
