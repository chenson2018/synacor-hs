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

-- TODO:
--  for step and assembly, check all bounds by using MaybeT

-- read a binary file to 16-bit words
readBinary :: String -> IO [Word16]
readBinary file =
  do
    n <- fromInteger . toInteger . fileSize <$> getFileStatus file
    let readInts = runGet $ replicateM (n `div` 2) getWord16le
    readInts . BL.fromChunks . (: []) <$> BS.readFile file

data VM = VM
  { memory :: M.Map Int Int,
    ptr :: Int,
    stack :: [Int],
    halted :: Bool,
    input :: [Char]
  }

-- initialize a VM from a binary
fromBinary :: Bool -> [Word16] -> VM
fromBinary auto bin = VM {memory, ptr = 0, stack = [], halted = False, input}
  where
    input = if auto then solution else []
    memory = M.fromList $ zip [0 .. 32775] $ map (fromInteger . toInteger) bin ++ repeat 0

-- given a value, interpret it as either a memory literal or register
interpMemory :: M.Map Int Int -> Int -> Int
interpMemory memory val
  | val < 32768 = val
  | otherwise = memory M.! val

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

-- width of each instruction, including the opcode
width :: Opcode -> Int
width Halt = 1
width Set = 3
width Push = 2
width Pop = 2
width Eq = 4
width Gt = 4
width Jmp = 2
width Jt = 3
width Jf = 3
width Add = 4
width Mult = 4
width Mod = 4
width And = 4
width Or = 4
width Not = 3
width Rmem = 3
width Wmem = 3
width Call = 2
width Ret = 1
width Out = 2
width In = 2
width Noop = 1

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

-- an iteration of the virtual machine
step :: VM -> IO VM
step vm =
  do
    let opcode = toEnum $ memory vm M.! ptr vm

    -- input is placed first, in case it changes the VM via an admin command!
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
    let ptr' = ptr + width opcode

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

-- iterate until the VM halts
untilHalt :: VM -> IO VM
untilHalt vm@(VM {halted = True}) = return vm
untilHalt vm = step vm >>= untilHalt

-- a function for printing assembly from a binary

{- ORMOLU_DISABLE -}

assembly :: Int -> [Word16] -> IO ()
assembly _ [] = return ()
assembly ptr (o : xs) =
  if o <= 21
    then
      let opcode :: Opcode = (toEnum . fromInteger . toInteger) o in 

      -- print for each airty
      let p0 tl       = putStrLn (printf "%06d: %s"          ptr (show opcode)                     ) >> assembly (ptr + 1) tl in
      let p1 tl a     = putStrLn (printf "%06d: %s %s"       ptr (show opcode) (md a)              ) >> assembly (ptr + 2) tl in 
      let p2 tl a b   = putStrLn (printf "%06d: %s %s %s"    ptr (show opcode) (md a) (md b)       ) >> assembly (ptr + 3) tl in
      let p3 tl a b c = putStrLn (printf "%06d: %s %s %s %s" ptr (show opcode) (md a) (md b) (md c)) >> assembly (ptr + 4) tl in

      case (width opcode, xs) of
           (1, tl) -> p0 tl
           (2, a : tl) -> p1 tl a
           (3, a : b : tl) -> p2 tl a b
           (4, a : b : c : tl) -> p3 tl a b c
    else 
      putStrLn (printf "%06d: data %s" ptr (md o)) >> assembly (ptr + 1) xs
    where
      md val | o == 19 = 
                let c :: Char = (toEnum . fromInteger . toInteger) val in 
                if c == '\n' then "'\\n'" else printf "'%c'" c
             | val < 32768 = show val
             | otherwise = printf "$%d" (val - 32768)

{- ORMOLU_ENABLE -}

-- precomputed solution
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
