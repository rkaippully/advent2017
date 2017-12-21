module Advent.Types where

import Data.ByteString.Lazy.Char8 (ByteString)

data Day = AllDays
         | Day1
         | Day2
         | Day3
         deriving (Show, Enum, Bounded)

data Part = AllParts
          | Part1
          | Part2
          deriving (Show, Enum, Bounded)

data Problem = Problem {
  name :: String
  , run :: ByteString -> ByteString }
