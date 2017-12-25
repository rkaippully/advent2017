{-# LANGUAGE OverloadedStrings #-}

module Advent.Day7 (day7part1, day7part2) where

import           Advent.Types (Problem(Problem))
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable (foldl')
import           Data.HashMap.Lazy as HM (HashMap, empty, insert, foldlWithKey', lookup, keys)
import           Data.Maybe (isJust, fromMaybe, fromJust)
import           Text.Parsec (Parsec, runParser, many1, skipMany1, optionMaybe, sepBy)
import           Text.Parsec.Char (alphaNum, space, char, digit, string)

type ProgName = BS.ByteString

data ProgInfo = ProgInfo {
  progName :: ProgName
  , progWeight :: BS.ByteString
  , progChildren :: [ProgName]
  , progParent :: Maybe ProgName }
  deriving (Show)

type ProgTree = HashMap ProgName ProgInfo

parseLine :: BS.ByteString -> ProgInfo
parseLine s =
  case runParser lineParser () "" s of
    Left e  -> error $ show e
    Right v -> v
  where
    lineParser :: Parsec BS.ByteString () ProgInfo
    lineParser = do
      name <- many1 alphaNum
      skipMany1 space
      _ <- char '('
      weight <- many1 digit
      _ <- char ')'
      childrenMaybe <- optionMaybe $ do
        skipMany1 space
        _ <- string "->"
        skipMany1 space
        many1 alphaNum `sepBy` (char ',' >> many1 space)
      return ProgInfo {
        progName = BS.pack name
        , progWeight = BS.pack weight
        , progChildren = BS.pack <$> fromMaybe [] childrenMaybe
        , progParent = Nothing }

mkTree :: BS.ByteString -> ProgTree -> ProgTree
mkTree str = let info = parseLine str
             in insert (progName info) info

fillParent :: ProgTree -> ProgTree
fillParent t = foldlWithKey' f empty t
  where
    f :: ProgTree -> ProgName -> ProgInfo -> ProgTree
    f tree parent info = foldl' (g parent) tree (progChildren info)

    g :: ProgName -> ProgTree -> ProgName -> ProgTree
    g parent tree child = case HM.lookup child t of
                            Nothing -> error $ "Could not find child " ++ show child
                            Just info -> insert child info{progParent = Just parent} tree

progTree :: BS.ByteString -> ProgTree
progTree = fillParent . foldr mkTree empty . BS.lines

parentOf :: ProgTree -> ProgName -> Maybe ProgName
parentOf tree name = HM.lookup name tree >>= progParent

rootOf :: ProgTree -> ProgName
rootOf tree = fromJust $ last $ takeWhile isJust $ iterate (>>= parentOf tree) $ Just . head $ keys tree

day7part1 :: Problem
day7part1 = Problem "day7part1" $ rootOf . progTree

day7part2 :: Problem
day7part2 = Problem "day7part2" $ \s ->
  let
    tree = progTree s
    root = rootOf tree
  in
    undefined
