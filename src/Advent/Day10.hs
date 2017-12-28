module Advent.Day10 (day10part1, day10part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString, fromByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable (foldl')


data Machine = Machine {
  size       :: Int    -- length of the list
  , list     :: [Int]  -- the circular (infinite) list
  , position :: Int    -- the head is supposed to be at this position in the list
  , skipSize :: Int }
  deriving (Show)

machineToList :: Machine -> [Int]
machineToList machine = take s $ drop (s - p) (list machine)
  where
    s = size machine
    p = position machine

runStep :: Machine -> Int -> Machine
runStep machine len =
  let
    s = size machine
    ss = skipSize machine
    pos = position machine
    (l, l') = splitAt len (list machine)
  in
    Machine {
      size = s
      , list = drop (len + ss) $ cycle $ reverse l ++ take (s - len) l'
      , position = (pos + len + ss) `mod` s
      , skipSize = ss + 1 }

day10part1 :: Problem
day10part1 = Problem "day10part1" $ \s ->
  let
    [[n], lengths] = (map fromByteString . BS.split ',') <$> BS.lines s
    machine = Machine {size = n, list = cycle [0..(n - 1)], position = 0, skipSize = 0}
    x:y:_ = machineToList $ foldl' runStep machine lengths
  in
    toByteString (x * y)

day10part2 :: Problem
day10part2 = Problem "day10part2" undefined
