module Advent.Day15 (day15part1, day15part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (fromByteString, toByteString)
import           Data.Bits ((.&.))
import qualified Data.ByteString.Char8 as BS
import           Data.Word (Word64)


type Generator = Word64 -> [Word64]

generator :: Word64 -> Generator
generator factor start = tail $ iterate gen start
  where
    gen :: Word64 -> Word64
    gen x = (x * factor) `mod` 2147483647

generatorA :: Generator
generatorA = generator 16807

generatorB :: Generator
generatorB = generator 48271

judge :: [Word64] -> [Word64] -> Int
judge seqA seqB = length $ filter match $ take 40000000 $ zip seqA seqB
  where
    match :: (Word64, Word64) -> Bool
    match (x, y) = x .&. 65535 == y .&. 65535

day15part1 :: Problem
day15part1 = Problem "day15part1" $ \s ->
  let
    [startA, startB] = fromByteString <$> BS.words s
  in
    toByteString $ judge (generatorA startA) (generatorB startB)

day15part2 :: Problem
day15part2 = Problem "day15part2" undefined
