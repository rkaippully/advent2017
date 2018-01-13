module Advent where

import Advent.Day01 (day01part1, day01part2)
import Advent.Day02 (day02part1, day02part2)
import Advent.Day03 (day03part1, day03part2)
import Advent.Day04 (day04part1, day04part2)
import Advent.Day05 (day05part1, day05part2)
import Advent.Day06 (day06part1, day06part2)
import Advent.Day07 (day07part1, day07part2)
import Advent.Day08 (day08part1, day08part2)
import Advent.Day09 (day09part1, day09part2)
import Advent.Day10 (day10part1, day10part2)
import Advent.Day11 (day11part1, day11part2)
import Advent.Day12 (day12part1, day12part2)
import Advent.Day13 (day13part1, day13part2)
import Advent.Day14 (day14part1, day14part2)
import Advent.Day15 (day15part1, day15part2)

import Advent.Types (Day(AllDays), Part(AllParts, Part1, Part2), Problem(name, run))
import Control.Monad (forM_, when)
import Data.ByteString.Char8 (hGetContents, unpack)
import Data.List (isSuffixOf)
import Data.Semigroup ((<>))
import System.Directory (listDirectory)
import System.IO (IOMode(ReadMode), withFile)


allProblems :: [Problem]
allProblems = [day01part1, day01part2
              , day02part1, day02part2
              , day03part1, day03part2
              , day04part1, day04part2
              , day05part1, day05part2
              , day06part1, day06part2
              , day07part1, day07part2
              , day08part1, day08part2
              , day09part1, day09part2
              , day10part1, day10part2
              , day11part1, day11part2
              , day12part1, day12part2
              , day13part1, day13part2
              , day14part1, day14part2
              , day15part1, day15part2
              ]

filterIndex :: (Int -> Bool) -> [a] -> [a]
filterIndex _ [] = []
filterIndex f xs = filter' 0 xs
  where
    filter' :: Int -> [a] -> [a]
    filter' _ [] = []
    filter' idx (y:ys) | f idx     = y : filter' (idx + 1) ys
                       | otherwise = filter' (idx + 1) ys

dayIdx :: Day -> Int
dayIdx day = (fromEnum day - 1) * 2

problemsOf :: (Day, Part) -> [Problem]
problemsOf (AllDays, AllParts) = allProblems
problemsOf (AllDays, Part1) = filterIndex even allProblems
problemsOf (AllDays, Part2) = filterIndex odd allProblems
problemsOf (day, AllParts) = [allProblems!!dayIdx day, allProblems!!(dayIdx day + 1)]
problemsOf (day, Part1) = [allProblems!!dayIdx day]
problemsOf (day, Part2) = [allProblems!!(dayIdx day + 1)]

execProblems :: (Day, Part) -> IO ()
execProblems daysAndParts = mapM_ execProblem (problemsOf daysAndParts)

execProblem :: Problem -> IO ()
execProblem problem = do
  putStrLn $ "\n" <> name problem <> ":"
  files <- listDirectory $ "test-data/" <> name problem
  forM_ files $ \file -> when (".data" `isSuffixOf` file) $ do
    let filename = "test-data/" <> name problem <> "/" <> file
    withFile filename ReadMode $ \dataHandle ->
      withFile (filename <> ".expected") ReadMode $ \expectedHandle -> do
        putStr $ file <> ": "
        test <- hGetContents dataHandle
        expected <- hGetContents expectedHandle
        let actual = run problem test
        if actual == expected
          then putStrLn "Pass"
          else putStrLn $ "Fail"
                          <> "\nTestdata : " <> unpack test
                          <> "\nExpected : " <> unpack expected
                          <> "\nActual   : " <> unpack actual
