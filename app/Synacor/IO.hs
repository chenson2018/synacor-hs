module Synacor.IO where

import Control.Monad (replicateM)
import Data.Binary (Word16)
import Data.Binary.Get (getWord16le, runGet)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import System.Posix (fileSize, getFileStatus)

readBinary :: String -> IO [Word16]
readBinary file =
  do
    n <- fromInteger . toInteger . fileSize <$> getFileStatus file
    let readInts = runGet $ replicateM (n `div` 2) getWord16le
    readInts . BL.fromChunks . (: []) <$> BS.readFile file
