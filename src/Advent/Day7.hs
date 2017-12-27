module Advent.Day7 (day7part1, day7part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString)
import           Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable (msum)
import           Data.HashMap.Lazy as HM (HashMap, fromList, keys, filter, lookup)
import           Data.List (groupBy)
import           Data.Maybe (fromMaybe, isJust, fromJust, listToMaybe)
import           Data.Tree (Tree(Node), unfoldTree, rootLabel)
import           Text.Parsec (Parsec, runParser, many1, skipMany1, optionMaybe, sepBy)
import           Text.Parsec.Char (alphaNum, space, char, digit, string)


type ProgName = BS.ByteString
type ProgWeight = Int
type ProgNode = (ProgName, ProgWeight)

data ProgInfo = ProgInfo {
  progName :: ProgName
  , progWeight :: ProgWeight
  , progChildren :: [ProgName]}
  deriving (Show)

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
        , progWeight = read weight
        , progChildren = BS.pack <$> fromMaybe [] childrenMaybe}

progTree :: BS.ByteString -> Maybe (Tree ProgNode)
progTree s = mkTree $ toMap $ parseLine <$> BS.lines s
  where
    toMap :: [ProgInfo] -> HashMap ProgName (ProgNode, [ProgName])
    toMap infos = fromList $ map (\i -> (progName i, ((progName i, progWeight i), progChildren i))) infos

    rootKey :: HashMap ProgName (ProgNode, [ProgName]) -> Maybe ProgName
    rootKey m = case takeWhile isJust $ iterate (>>= parentOf m) $ Just . head $ keys m of
                  [] -> Nothing
                  xs -> last xs

    parentOf :: HashMap ProgName (ProgNode, [ProgName]) -> ProgName -> Maybe ProgName
    parentOf m k = listToMaybe $ keys $ HM.filter (\(_, cs) -> k `elem` cs) m

    mkTree :: HashMap ProgName (ProgNode, [ProgName]) -> Maybe (Tree ProgNode)
    mkTree m = unfoldTree (\k -> fromJust $ HM.lookup k m) <$> rootKey m

day7part1 :: Problem
day7part1 = Problem "day7part1" $ \s ->
  case progTree s of
    Nothing -> error "Empty tree"
    Just x  -> fst $ rootLabel x

day7part2 :: Problem
day7part2 = Problem "day7part2" $ \s ->
  case progTree s of
    Nothing   -> error "Empty tree"
    Just tree -> toByteString $ fromJust $ balance $ treeSum tree

  where
    treeSum :: Tree ProgNode -> Tree (ProgWeight, ProgWeight)
    treeSum (Node (_, w) []) = Node (w, w) []
    treeSum (Node (_, w) cs) = Node (w, w + s) (map treeSum cs)
      where
        cs' = map treeSum cs
        s = sum $ map summedWeight cs'

    summedWeight :: Tree (ProgWeight, ProgWeight) -> ProgWeight
    summedWeight = snd . rootLabel

    -- The current node is unbalanced if its siblings have a different summed weight
    -- but all its children have the same weight
    balance :: Tree (ProgWeight, ProgWeight) -> Maybe ProgWeight
    balance (Node _ []) = Nothing
    balance (Node _ cs) = case groupBy (\(_, w1) (_, w2) -> w1 == w2) (map rootLabel cs) of
                            [_]                        -> childrenResult
                            [[(w, sw)], (_, sw'):_:_]  -> childrenResult <|> Just (w + sw' - sw)
                            [(_, sw'):_:_, [(w, sw)]]  -> childrenResult <|> Just (w + sw' - sw)
                            [(_, sw'):_, [(w, sw)], _] -> childrenResult <|> Just (w + sw' - sw)
                            x                          -> error $ "Invalid pattern - " ++ show x
      where
        childrenResult :: Maybe ProgWeight
        childrenResult = msum $ map balance cs
