module Synacor where

import Control.Monad (replicateM)
import Data.Binary (Word16)
import Data.Binary.Get (getWord16le, runGet)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import System.Posix (fileSize, getFileStatus)

readBinary :: String -> IO [Word16]
readBinary file =
  do
    n <- fromInteger . toInteger . fileSize <$> getFileStatus file
    let readInts = runGet $ replicateM (n `div` 2) getWord16le
    readInts . BL.fromChunks . (: []) <$> BS.readFile file

-- TODO:
--  do I really want a map here?
--  Int vs Integer?
--  should registers and memory be combined like this?
--  I think monad transformers would help me to combine IO/State/Maybe monads
--    is the Flexible contexts extension relevant?
data VM = VM
  { memory :: M.Map Integer Integer,
    ptr :: Integer,
    stack :: [Integer]
  }
  deriving (Show)

fromBinary :: [Word16] -> VM
fromBinary bin = VM {memory, ptr = 0, stack = []}
  where
    memory = M.fromList $ zip [0 .. 32775] $ map toInteger bin ++ repeat 0
