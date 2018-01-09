{-# LANGUAGE OverloadedStrings #-}

module Advent.Day14 (day14part1, day14part2) where

import Advent.KnotHash (knotHash)
import Advent.Types (Problem(Problem))
import Advent.Util (toByteString)
import Data.Bits (popCount, testBit)
import Data.Foldable (foldl')
import Data.HashSet as HS (HashSet, empty, singleton, member, null, toList, delete)
import Data.List.Index (ifoldMap)
import Data.Semigroup ((<>))
import Data.Word (Word8)


day14part1 :: Problem
day14part1 = Problem "day14part1" $ \s ->
  let
    inputs = map (((s <> "-") <>) . toByteString) ([0..127] :: [Int])
    hashes = concatMap (knotHash 256) inputs
  in
    toByteString $ sum $ popCount <$> hashes

type Coord = (Int, Int)

toGrid :: [[Word8]] -> HashSet Coord
toGrid = ifoldMap f
  where
    f :: Int -> [Word8] -> HashSet Coord
    f row = ifoldMap (g row)

    g :: Int -> Int -> Word8 -> HashSet Coord
    g row idx w = foldMap (\i -> if w `testBit` i then singleton (idx*8 + (7 - i), row) else empty) [0..7]

regionCount :: HashSet Coord -> Int
regionCount grid | HS.null grid = 0
                 | otherwise    = let x = head $ toList grid
                                  in 1 + regionCount (removeRegion grid x)

removeRegion :: HashSet Coord -> Coord -> HashSet Coord
removeRegion g c = let g' = c `delete` g
                   in foldl' removeRegion g' $ neighbors g' c

neighbors :: HashSet Coord -> Coord -> [Coord]
neighbors g (x, y) = filter (`member` g) [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

day14part2 :: Problem
day14part2 = Problem "day14part2" $ \s ->
  let
    inputs = map (((s <> "-") <>) . toByteString) ([0..127] :: [Int])
    grid = toGrid $ map (knotHash 256) inputs
  in
    toByteString $ regionCount grid
