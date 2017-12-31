{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day12 (day12part1, day12part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.HashMap.Lazy as HM (HashMap, lookupDefault)
import qualified Data.HashSet as HS (HashSet, size, singleton, empty, null, map, unions, union, difference)
import           Data.Hashable (Hashable)
import           GHC.Exts (fromList, toList)
import           Text.Parsec (Parsec, runParser, sepBy, char, many1, skipMany1, space, string, alphaNum)


newtype Graph e = Graph { unGraph :: HashMap e (HS.HashSet e) }
  deriving (Show)

type Element = BS.ByteString

parseInput :: BS.ByteString -> Graph Element
parseInput str =
  case runParser graph () "" str of
    Left e  -> error $ show e
    Right v -> v
  where
    word :: Parsec BS.ByteString () String
    word = many1 alphaNum

    skipSpaces :: Parsec BS.ByteString () ()
    skipSpaces = skipMany1 space

    graph :: Parsec BS.ByteString () (Graph Element)
    graph = do
      xs <- entry `sepBy` char '\n'
      return $ Graph $ fromList xs

    entry :: Parsec BS.ByteString () (Element, HS.HashSet Element)
    entry = do
      x <- word
      skipSpaces
      _ <- string "<->"
      skipSpaces
      xs <- word `sepBy` (char ',' >> skipSpaces)
      return (BS.pack x, fromList $ map BS.pack xs)

groupOf :: forall e. (Eq e, Hashable e) => e -> Graph e -> HS.HashSet e
groupOf e g = groups HS.empty (HS.singleton e)
  where
    groups :: HS.HashSet e -> HS.HashSet e -> HS.HashSet e
    groups prev curr = let s = HS.difference curr prev
                       in if HS.null s
                          then curr
                          else groups curr $ HS.union curr $ reachables s

    reachables :: HS.HashSet e -> HS.HashSet e
    reachables = mapcat (\x -> lookupDefault HS.empty x $ unGraph g)

    mapcat :: (Eq b, Hashable b) => (a -> HS.HashSet b) -> HS.HashSet a -> HS.HashSet b
    mapcat f s = HS.unions $ toList $ HS.map f s

day12part1 :: Problem
day12part1 = Problem "day12part1" $ \s ->
  let
    graph = parseInput s
  in
    toByteString $ HS.size $ groupOf "0" graph

day12part2 :: Problem
day12part2 = Problem "day12part2" undefined
