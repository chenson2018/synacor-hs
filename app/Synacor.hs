module Synacor where

import Control.Monad (replicateM, when)
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
  { memory :: M.Map Int Int,
    ptr :: Int,
    stack :: [Int],
    halted :: Bool
  }
  deriving (Show)

fromBinary :: [Word16] -> VM
fromBinary bin = VM {memory, ptr = 0, stack = [], halted = False}
  where
    memory = M.fromList $ zip [0 .. 32775] $ map (fromInteger . toInteger) bin ++ repeat 0

-- eventually should handle IO, Maybe, and State
opLen :: Int -> Int
opLen 0 = 1
opLen 19 = 2
opLen 21 = 1

step :: VM -> IO VM
step VM {memory, ptr, stack} =
  do
    let opcode = memory M.! ptr
    let ptr' = ptr + opLen opcode
    let halted' = opcode == 0
    when
      (opcode == 19)
      ( do
          let a = memory M.! (ptr + 1)
          let char :: Char = toEnum a
          putStr [char]
      )
    return $ VM {memory, ptr = ptr', stack, halted = halted'}

untilHalt :: VM -> IO VM
untilHalt vm@(VM {halted = True}) = return vm
untilHalt vm = step vm >>= untilHalt
