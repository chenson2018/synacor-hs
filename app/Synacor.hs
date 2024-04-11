module Synacor where

import Control.Monad (replicateM, when)
import Data.Binary (Word16)
import Data.Binary.Get (getWord16le, runGet)
import Data.Bits (complement, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import System.IO (hFlush, stdout)
import System.Posix (fileSize, getFileStatus)
import Text.Printf (printf)

readBinary :: String -> IO [Word16]
readBinary file =
  do
    n <- fromInteger . toInteger . fileSize <$> getFileStatus file
    let readInts = runGet $ replicateM (n `div` 2) getWord16le
    readInts . BL.fromChunks . (: []) <$> BS.readFile file

{- ORMOLU_DISABLE -}

-- TODO : maybeT here?
assembly :: Int -> [Word16] -> IO ()
assembly _ [] = return ()
assembly ptr (o : xs) =
  if o <= 21
    then
      let opcode :: Opcode = (toEnum . fromInteger . toInteger) o in 
      
      -- print for each airty
      let p0 op tl       = putStrLn (printf "%d: %s"          ptr (show op)                     ) >> assembly (ptr + 1) tl in
      let p1 op tl a     = putStrLn (printf "%d: %s %s"       ptr (show op) (md a)              ) >> assembly (ptr + 2) tl in 
      let p2 op tl a b   = putStrLn (printf "%d: %s %s %s"    ptr (show op) (md a) (md b)       ) >> assembly (ptr + 3) tl in
      let p3 op tl a b c = putStrLn (printf "%d: %s %s %s %s" ptr (show op) (md a) (md b) (md c)) >> assembly (ptr + 4) tl in

      case (opLen opcode, xs) of
           (1, tl) -> p0 opcode tl
           (2, a : tl) -> p1 opcode tl a
           (3, a : b : tl) -> p2 opcode tl a b
           (4, a : b : c : tl) -> p3 opcode tl a b c
    else 
      putStrLn (printf "%d: data %s" ptr (md o)) >> assembly (ptr + 1) xs
    where
      md val | val < 32768 = show val
             | otherwise = printf "$%d" (val - 32768)

{- ORMOLU_ENABLE -}

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
    halted :: Bool,
    input :: [Char]
  }

-- show just the registers when printing
instance Show VM where
  show VM {memory, ptr, stack} =
    unlines $
      zipWith
        (++)
        (map (++ ": ") ["ptr", "registers", "stack"])
        [show ptr, show registers, show stack]
    where
      registers = map (memory M.!) [32768 .. 32775]

-- defining these for a bit of readability
data Opcode
  = Halt
  | Set
  | Push
  | Pop
  | Eq
  | Gt
  | Jmp
  | Jt
  | Jf
  | Add
  | Mult
  | Mod
  | And
  | Or
  | Not
  | Rmem
  | Wmem
  | Call
  | Ret
  | Out
  | In
  | Noop
  deriving (Show, Enum, Eq)

fromBinary :: Bool -> [Word16] -> VM
fromBinary auto bin = VM {memory, ptr = 0, stack = [], halted = False, input}
  where
    input = if auto then solution else []
    memory = M.fromList $ zip [0 .. 32775] $ map (fromInteger . toInteger) bin ++ repeat 0

-- TODO: eventually should handle IO and Maybe (and State?)
opLen :: Opcode -> Int
opLen Halt = 1
opLen Set = 3
opLen Push = 2
opLen Pop = 2
opLen Eq = 4
opLen Gt = 4
opLen Jmp = 2
opLen Jt = 3
opLen Jf = 3
opLen Add = 4
opLen Mult = 4
opLen Mod = 4
opLen And = 4
opLen Or = 4
opLen Not = 3
opLen Rmem = 3
opLen Wmem = 3
opLen Call = 2
opLen Ret = 1
opLen Out = 2
opLen In = 2
opLen Noop = 1

-- given a value, interpret it as either a memory literal or register
interpMemory :: M.Map Int Int -> Int -> Int
interpMemory memory val
  | val < 32768 = val
  | otherwise = memory M.! val

-- take user input, including admin commands that can mutate the VM
handleInput :: VM -> IO VM
handleInput vm@(VM {memory, input = []}) =
  do
    putStr "> "
    hFlush stdout
    s <- getLine
    case s of
      "admin" -> do print vm; handleInput vm
      "set reg" ->
        do
          putStr "register to edit: "
          hFlush stdout
          reg <- (+ 32768) . read <$> getLine
          putStr "value: "
          hFlush stdout
          val <- read <$> getLine
          handleInput vm {memory = M.insert reg val memory}
      "set ptr" ->
        do
          putStr "value: "
          hFlush stdout
          ptr <- read <$> getLine
          step vm {ptr}
      _ -> return (vm {input = s ++ ['\n']})
handleInput vm = return vm

step :: VM -> IO VM
step vm =
  do
    -- TODO check failure
    let opcode = toEnum $ memory vm M.! ptr vm

    -- input is placed first, in case in changes the VM via an admin command!
    vm'@(VM {memory, ptr, stack, input}) <- case opcode of
      In -> handleInput vm
      _ -> return vm

    -- this is lazy, cool!
    let a_imm = memory M.! (ptr + 1)
    let b_imm = memory M.! (ptr + 2)
    let c_imm = memory M.! (ptr + 3)
    let a_val = interpMemory memory a_imm
    let b_val = interpMemory memory b_imm
    let c_val = interpMemory memory c_imm

    when
      (opcode == Out)
      ( do
          let char :: Char = toEnum a_val
          putStr [char]
      )

    -- this is just for readability
    let set addr val = M.insert addr val memory
    let ptr' = ptr + opLen opcode

    let vm'' =
          ( case opcode of
              Halt -> vm' {halted = True}
              Set -> vm' {memory = set a_imm b_val, ptr = ptr'}
              Push -> vm' {stack = a_val : stack, ptr = ptr'}
              Pop ->
                let hd : stack' = stack
                 in vm' {memory = set a_imm hd, stack = stack', ptr = ptr'}
              Eq ->
                let val = if b_val == c_val then 1 else 0
                 in vm' {memory = set a_imm val, ptr = ptr'}
              Gt ->
                let val = if b_val > c_val then 1 else 0
                 in vm' {memory = set a_imm val, ptr = ptr'}
              Jmp -> vm' {ptr = a_val}
              Jt -> vm' {ptr = if a_val /= 0 then b_imm else ptr'}
              Jf -> vm' {ptr = if a_val == 0 then b_imm else ptr'}
              Add -> vm' {memory = set a_imm $ (b_val + c_val) `mod` 32768, ptr = ptr'}
              Mult -> vm' {memory = set a_imm $ (b_val * c_val) `mod` 32768, ptr = ptr'}
              Mod -> vm' {memory = set a_imm $ b_val `mod` c_val, ptr = ptr'}
              And -> vm' {memory = set a_imm $ b_val .&. c_val, ptr = ptr'}
              Or -> vm' {memory = set a_imm $ b_val .|. c_val, ptr = ptr'}
              Not -> vm' {memory = set a_imm $ complement b_val `mod` 32768, ptr = ptr'}
              Rmem -> vm' {memory = set a_imm $ interpMemory memory $ memory M.! b_val, ptr = ptr'}
              Wmem -> vm' {memory = set a_val b_val, ptr = ptr'}
              Call -> vm' {stack = ptr' : stack, ptr = a_val}
              Ret ->
                case stack of
                  [] -> vm' {halted = True}
                  hd : stack' -> vm' {ptr = hd, stack = stack'}
              In ->
                let char :: Int = fromEnum $ head input
                 in vm' {memory = set a_imm char, input = tail input, ptr = ptr'}
              Out -> vm' {ptr = ptr'}
              Noop -> vm' {ptr = ptr'}
          )

    return vm''

untilHalt :: VM -> IO VM
untilHalt vm@(VM {halted = True}) = return vm
untilHalt vm = step vm >>= untilHalt

solution :: String
solution =
  unlines
    [ "take tablet",
      "use tablet",
      "doorway",
      "north",
      "north",
      "bridge",
      "continue",
      "down",
      "east",
      "take empty lantern",
      "west",
      "west",
      "passage",
      "ladder",
      "west",
      "north",
      "south",
      "north",
      "take can",
      "use can",
      "use lantern",
      "west",
      "ladder",
      "darkness",
      "continue",
      "west",
      "west",
      "west",
      "west",
      "north",
      "take red coin",
      "north",
      "east",
      "take concave coin",
      "down",
      "take corroded coin",
      "up",
      "west",
      "west",
      "take blue coin",
      "up",
      "take shiny coin",
      "down",
      "east",
      "use blue coin",
      "use red coin",
      "use shiny coin",
      "use concave coin",
      "use corroded coin",
      "north",
      "take teleporter",
      "use teleporter"
    ]
