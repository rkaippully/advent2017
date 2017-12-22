module Advent.Util where

import qualified Data.ByteString.Char8 as BS

toByteString :: Show a => a -> BS.ByteString
toByteString = BS.pack . show

fromByteString :: Read a => BS.ByteString -> a
fromByteString = read . BS.unpack

feed :: Monad m => [a -> m a] -> a -> m a
feed [] x = return x
feed (f:fs) x = f x >>= feed fs
