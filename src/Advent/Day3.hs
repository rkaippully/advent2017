module Advent.Day3 where

import           Advent.Types (Problem(Problem))
import qualified Data.ByteString.Lazy.Char8 as BS

data Direction = Asc | Desc deriving (Eq)

manhattanDistances :: [Int]
manhattanDistances = 0 : concatMap gen [1..]
  where
    -- generate the manhattan distance list for `lvl`
    gen :: Int -> [Int]
    gen lvl = take (lvl * 8) $ cycle $ [lvl*2-1, lvl*2-2..lvl] ++ [lvl+1..lvl*2]

day3part1 :: Problem
day3part1 = Problem "day3part1" $ \s ->
  let
    n = read $ BS.unpack s
  in
    BS.pack $ show $ manhattanDistances!!(n - 1)

day3part2 :: Problem
day3part2 = Problem "day3part2" undefined
