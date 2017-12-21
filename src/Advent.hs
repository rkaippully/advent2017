module Advent where

import Advent.Day1 (day1part1, day1part2)
import Advent.Day2 (day2part1, day2part2)
import Advent.Day3 (day3part1, day3part2)
import Advent.Day4 (day4part1, day4part2)

import Advent.Types (Day(AllDays), Part(AllParts, Part1, Part2), Problem(name, run))
import Control.Monad (forM_, when)
import Data.ByteString.Char8 (hGetContents, unpack)
import Data.List (isSuffixOf)
import Data.Semigroup ((<>))
import System.Directory (listDirectory)
import System.IO (IOMode(ReadMode), withFile)


allProblems :: [Problem]
allProblems = [day1part1, day1part2
              , day2part1, day2part2
              , day3part1, day3part2
              , day4part1, day4part2]

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
