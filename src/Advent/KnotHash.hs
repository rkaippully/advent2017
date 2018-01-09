module Advent.KnotHash (simpleHash, knotHash, hashToHex) where

import           Data.Bits (xor)
import           Data.ByteString.Builder (Builder, toLazyByteString, word8HexFixed)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (foldl')
import           Data.Monoid (mempty)
import           Data.Semigroup ((<>))
import           Data.Word (Word8)


data Machine = Machine {
  size       :: Int      -- length of the list
  , list     :: [Word8]  -- the circular (infinite) list
  , position :: Int      -- the head is supposed to be at this position in the list
  , skipSize :: Int }
  deriving (Show)

machineToList :: Machine -> [Word8]
machineToList machine = take s $ drop (s - p) (list machine)
  where
    s = size machine
    p = position machine

runStep :: Machine -> Int -> Machine
runStep machine len =
  let
    s = size machine
    ss = skipSize machine
    pos = position machine
    (l, l') = splitAt len (list machine)
  in
    Machine {
      size = s
      , list = drop (len + ss) $ cycle $ reverse l ++ take (s - len) l'
      , position = (pos + len + ss) `mod` s
      , skipSize = ss + 1 }

denseHash :: [Word8] -> [Word8]
denseHash [] = []
denseHash xs =
  let
    (block, rest) = splitAt 16 xs
  in
    foldl1 xor block : denseHash rest

hashToHex :: [Word8] -> BS.ByteString
hashToHex = LBS.toStrict . toLazyByteString . f
  where
    f :: [Word8] -> Builder
    f = foldl' (\acc c -> acc <> word8HexFixed c) mempty

simpleHash :: Int -> [Int] -> Int
simpleHash n lengths =
  let
    machine = Machine {size = n, list = cycle [0..fromIntegral (n - 1)], position = 0, skipSize = 0}
    x:y:_ = machineToList $ foldl' runStep machine lengths
  in
    fromIntegral x * fromIntegral y

knotHash :: Int -> BS.ByteString -> [Word8]
knotHash n str =
  let
    str' = BS.foldr (\c acc -> fromEnum c : acc) [17, 31, 73, 47, 23] str
    machine = Machine {size = n, list = cycle [0..fromIntegral (n - 1)], position = 0, skipSize = 0}
  in
    denseHash $ machineToList $ foldl' runStep machine $ take (64 * length str') $ cycle str'
