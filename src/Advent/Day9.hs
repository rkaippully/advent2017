module Advent.Day9 (day9part1, day9part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List (foldl')
import           Data.Tree (Tree(Node), levels, flatten)
import           Text.Parsec (Parsec, (<|>), runParser, sepBy, char, between, many, try, noneOf, anyChar)


data Element = Group
             | Garbage Int
             deriving (Show, Eq)

data GarbageChar = CancelledChar
                 | NormalChar
                 deriving (Show, Eq)

parseInput :: BS.ByteString -> Tree Element
parseInput str =
  case runParser group () "" str of
    Left e  -> error $ show e
    Right v -> v
  where
    group :: Parsec BS.ByteString () (Tree Element)
    group = do
      cs <- between (char '{') (char '}') (sepBy element $ char ',')
      return $ Node Group cs

    element :: Parsec BS.ByteString () (Tree Element)
    element = group <|> garbage

    garbage :: Parsec BS.ByteString () (Tree Element)
    garbage = do
      cs <- between (char '<') (char '>') (many garbageChar)
      return $ Node (Garbage $ length $ filter (== NormalChar) cs) []

    garbageChar :: Parsec BS.ByteString () GarbageChar
    garbageChar = try cancelledChar
                  <|> try normalChar

    cancelledChar :: Parsec BS.ByteString () GarbageChar
    cancelledChar = char '!' >> anyChar >> return CancelledChar

    normalChar :: Parsec BS.ByteString () GarbageChar
    normalChar = noneOf ">" >> return NormalChar

day9part1 :: Problem
day9part1 = Problem "day9part1" $
  toByteString . fst . foldl' f (0, 1) . levels . parseInput
  where
    f :: (Int, Int) -> [Element] -> (Int, Int)
    f (total, lvl) xs = (total + length (filter (== Group) xs) * lvl, lvl + 1)

day9part2 :: Problem
day9part2 = Problem "day9part2" $
  toByteString . foldl' f 0 . flatten . parseInput
  where
    f :: Int -> Element -> Int
    f acc (Garbage n) = acc  + n
    f acc Group = acc
