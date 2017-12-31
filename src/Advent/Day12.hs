{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day12 (day12part1, day12part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Graph (Graph, Vertex, graphFromEdges, dfs, dff)
import           Data.Maybe (fromJust)
import           Data.Tree (flatten)
import           Text.Parsec (Parsec, runParser, sepBy, char, many1, skipMany1, space, string, alphaNum)


type Element = BS.ByteString

parseInput :: BS.ByteString -> (Graph, Element -> Maybe Vertex)
parseInput str =
  case runParser graph () "" str of
    Left e  -> error $ show e
    Right v -> v
  where
    word :: Parsec BS.ByteString () String
    word = many1 alphaNum

    skipSpaces :: Parsec BS.ByteString () ()
    skipSpaces = skipMany1 space

    graph :: Parsec BS.ByteString () (Graph, Element -> Maybe Vertex)
    graph = do
      xs <- entry `sepBy` char '\n'
      let (g, _, f) = graphFromEdges xs
      return (g, f)

    entry :: Parsec BS.ByteString () (Element, Element, [Element])
    entry = do
      x <- BS.pack <$> word
      skipSpaces
      _ <- string "<->"
      skipSpaces
      xs <- map BS.pack <$> (word `sepBy` (char ',' >> skipSpaces))
      return (x, x, xs)

day12part1 :: Problem
day12part1 = Problem "day12part1" $ \s ->
  let
    (graph, elemToVertexMaybe) = parseInput s
  in
    toByteString $ length . flatten . head $ dfs graph [fromJust $ elemToVertexMaybe "0"]

day12part2 :: Problem
day12part2 = Problem "day12part2" $ \s ->
  let
    graph = fst $ parseInput s
  in
    toByteString $ length $ dff graph
