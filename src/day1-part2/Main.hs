module Main where

import Data.Char (digitToInt)

main :: IO ()
main = do
  s <- getLine
  let l = length s
      s' = cycle s
      s'' = drop (l `div` 2) s'
  print $ sum $ map (digitToInt . fst) $ filter (\(a, b) -> a == b) $ take l $ zip s' s''
