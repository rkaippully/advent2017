module Advent.Day13 (day13part1, day13part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString, fromByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable (find)
import           Data.Maybe (fromJust)


type Layer = Int
type Range = Int
type Scanner = (Layer, Range)

pairs :: [a] -> (a, a)
pairs [x, y] = (x, y)
pairs _      = undefined

{-
  Are we caught at a layer if we start with a specific delay?

  A scanning sequence at layer l with range r will look like this: 0,1,2,...,r-2,r-1,r-2,...,1,0,1,...
  Each repitition in the cycle has 2*(r-1) numbers so this can be generated by a (`mod` (r-1)*2) function.
-}
isCaught :: Int -> Scanner -> Bool
isCaught delay (l, r) = let x = r - 1
                        in abs (((l + delay - x) `mod` (x*2)) - x) == 0

readScanners :: BS.ByteString -> [Scanner]
readScanners s = (pairs . map fromByteString . BS.split ':') <$> BS.lines s

day13part1 :: Problem
day13part1 = Problem "day13part1" $ \s ->
  let
    -- Scanner level and ranges
    scanners = readScanners s
    -- Layers at which we are caught
    cs = filter (isCaught 0) scanners
  in
    toByteString $ sum $ map (uncurry (*)) cs

day13part2 :: Problem
day13part2 = Problem "day13part2" $ \s ->
  let
    -- Scanner level and ranges
    scanners = readScanners s

    -- Not caught at any level
    noneCaught :: Int -> Bool
    noneCaught d = all (not . isCaught d) scanners

    -- First layer at which we are not caught
    delay = fromJust $ find noneCaught [0..]
  in
    toByteString delay
