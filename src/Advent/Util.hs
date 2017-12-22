module Advent.Util where

import qualified Data.ByteString.Char8 as BS

toByteString :: Show a => a -> BS.ByteString
toByteString = BS.pack . show

fromByteString :: Read a => BS.ByteString -> a
fromByteString = read . BS.unpack
