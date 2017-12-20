module Main where

import Data.Char (digitToInt)

main :: IO ()
main = do
  s <- getLine
  let l = length s
      s' = cycle s
      s'' = tail s'
  print $ sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ take l $ zip s' s''
