{-# LANGUAGE DeriveDataTypeable #-}

module Synacor where

import Control.Monad (replicateM, when)
import Data.Binary (Word16)
import Data.Binary.Get (getWord16le, runGet)
import Data.Bits (complement, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Data
import Data.Foldable (toList)
import Data.Sequence qualified as S
import System.IO (hFlush, stdout)
import System.Posix (fileSize, getFileStatus)
import Text.Printf (PrintfArg, printf)

-- TODO:
--  for step and assembly, check all bounds by using MaybeT
--
--  better handle the non-exhaustive case in assembly
--    is nice for the admin command, but silently fails otherwise

-- read a binary file to 16-bit words
readBinary :: String -> IO [Word16]
readBinary file =
  do
    n <- fromInteger . toInteger . fileSize <$> getFileStatus file
    let readInts = runGet $ replicateM (n `div` 2) getWord16le
    readInts . BL.fromChunks . (: []) <$> BS.readFile file

data VM = VM
  { memory :: S.Seq Int,
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
    memory = S.fromList $ take 32776 $ map (fromInteger . toInteger) bin ++ repeat 0

-- given a value, interpret it as either a memory literal or register
interpMemory :: S.Seq Int -> Int -> Int
interpMemory memory val
  | val < 32768 = val
  | otherwise = S.index memory val

-- show just the registers when printing
instance Show VM where
  show VM {memory, ptr, stack} =
    unlines $
      zipWith
        (++)
        (map (++ ": ") ["ptr", "registers", "stack"])
        [show ptr, show registers, show stack]
    where
      registers = map (S.index memory) [32768 .. 32775]

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
  deriving (Typeable, Data, Enum, Eq)

instance Show Opcode where
  show = map toLower . showConstr . toConstr

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
          handleInput vm {memory = S.update reg val memory}
      "set ptr" ->
        do
          putStr "value: "
          hFlush stdout
          ptr' <- read <$> getLine
          step vm {ptr = ptr'}
      "peek" ->
        do
          putStr "start: "
          hFlush stdout
          start <- read <$> getLine
          putStr "stop: "
          hFlush stdout
          stop <- read <$> getLine
          putStrLn ""
          assembly True start $ filter (\k -> start <= k && k <= stop) $ toList memory
          handleInput vm
      _ -> return (vm {input = s ++ ['\n']})
handleInput vm = return vm

-- an iteration of the virtual machine
step :: VM -> IO VM
step vm =
  do
    let opcode = toEnum $ S.index (memory vm) (ptr vm)

    -- input is placed first, in case it changes the VM via an admin command!
    vm'@(VM {memory, ptr, stack, input}) <- case opcode of
      In -> handleInput vm
      _ -> return vm

    -- this is lazy, cool!
    let a_imm = S.index memory (ptr + 1)
    let b_imm = S.index memory (ptr + 2)
    let c_imm = S.index memory (ptr + 3)
    let a_val = interpMemory memory a_imm
    let b_val = interpMemory memory b_imm
    let c_val = interpMemory memory c_imm

    when
      (opcode == Out)
      (putChar $ toEnum a_val)

    -- this is just for readability
    let set addr val = S.update addr val memory
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
              Rmem -> vm' {memory = set a_imm $ interpMemory memory $ S.index memory b_val, ptr = ptr'}
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

assembly :: (Num t, Eq t, Ord t, Integral t, Show t, PrintfArg t) => Bool -> Int -> [t] -> IO ()
assembly _ _ [] = return ()
assembly str_start ptr (o : xs)
  | o == 19 =
      do
        let a : tl = xs
        -- is this the start of the string?
        when str_start $ putStr (printf "%06d: out \"" ptr)
        -- print the char
        let c :: Char = (toEnum . fromInteger . toInteger) a
        let s = if c == '\n' then "\\n" else [c]
        putStr s
        -- is this the end of the string?
        case tl of
          tl'@(19 : _) -> assembly False (ptr + 2) tl'
          tl'@(_ : _) -> putStr "\"\n" >> assembly True (ptr + 2) tl'
          [] -> putStr "\"\n"
  | o <= 21 =
      let opcode :: Opcode = (toEnum . fromInteger . toInteger) o in
      case (width opcode, xs) of
        (1,             tl) -> putStrLn (printf "%06d: %s"          ptr (show opcode)                     ) >> assembly True (ptr + 1) tl
        (2, a :         tl) -> putStrLn (printf "%06d: %s %s"       ptr (show opcode) (md a)              ) >> assembly True (ptr + 2) tl
        (3, a : b :     tl) -> putStrLn (printf "%06d: %s %s %s"    ptr (show opcode) (md a) (md b)       ) >> assembly True (ptr + 3) tl
        (4, a : b : c : tl) -> putStrLn (printf "%06d: %s %s %s %s" ptr (show opcode) (md a) (md b) (md c)) >> assembly True (ptr + 4) tl
        _ -> return ()
  | otherwise = putStrLn (printf "%06d: data %s" ptr (md o)) >> assembly True (ptr + 1) xs
  where
    md val
      | val < 32768 = show val
      | otherwise = printf "$%d" (val - 32768)

{- ORMOLU_DISABLE -}

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
