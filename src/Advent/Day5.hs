module Advent.Day5 (day5part1, day5part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString, fromByteString, feed)
import qualified Data.ByteString.Char8 as BS

data Instructions a = Instructions [a] a [a]

left :: Instructions a -> Maybe (Instructions a)
left (Instructions (l:ls) x rs) = Just $ Instructions ls l (x:rs)
left (Instructions [] _ _) = Nothing

right :: Instructions a -> Maybe (Instructions a)
right (Instructions ls x (r:rs)) = Just $ Instructions (x:ls) r rs
right (Instructions _ _ []) = Nothing

runProgram :: Instructions Int -> (Int -> Int) -> (Instructions Int, Int)
runProgram insns updater = go insns 0
  where
    go :: Instructions Int -> Int -> (Instructions Int, Int)
    go ins@(Instructions ls x rs) count =
      let
        f = if x < 0 then left else right
        fns = replicate (abs x) f
      in
        case feed fns (Instructions ls (updater x) rs) of
          Just insns' -> go insns' (count + 1)
          Nothing     -> (ins, count + 1)

mkInsns :: BS.ByteString -> Instructions Int
mkInsns s =
  let
    ns = map fromByteString $ BS.lines s
  in
    Instructions [] (head ns) (tail ns)

day5part1 :: Problem
day5part1 = Problem "day5part1" $ \s ->
  toByteString $ snd $ runProgram (mkInsns s) (+ 1)

day5part2 :: Problem
day5part2 = Problem "day5part2" $ \s ->
  toByteString $ snd $ runProgram (mkInsns s) (\offset -> if offset >= 3 then offset - 1 else offset + 1)
