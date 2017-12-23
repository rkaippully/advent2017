module Advent.Util where

import qualified Data.ByteString.Char8 as BS

toByteString :: Show a => a -> BS.ByteString
toByteString = BS.pack . show

fromByteString :: Read a => BS.ByteString -> a
fromByteString = read . BS.unpack

rotateLeft :: Int -> [a] -> [a]
rotateLeft _ [] = []
rotateLeft n xs = take l $ drop n (cycle xs)
  where
    l = length xs

rotateRight :: Int -> [a] -> [a]
rotateRight _ [] = []
rotateRight n xs = take l $ drop (l - n) (cycle xs)
  where
    l = length xs
