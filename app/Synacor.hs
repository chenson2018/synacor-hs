{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Synacor where

import Control.Lens
import Control.Monad (replicateM, when)
import Data.Binary (Word16)
import Data.Binary.Get (getWord16le, runGet)
import Data.Bits (complement, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Data
import Data.Foldable (toList)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Parsing
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
  { _memory :: S.Seq Int,
    _ptr :: Int,
    _stack :: [Int],
    _halted :: Bool,
    _input :: String,
    _solution :: [String],
    _bypass :: M.Map Int String
  }

$(makeLenses ''VM)

-- initialize a VM from a binary
fromBinary :: Bool -> [Word16] -> VM
fromBinary auto bin =
  VM
    { _memory = S.fromList $ take 32776 $ map (fromInteger . toInteger) bin ++ repeat 0,
      _ptr = 0,
      _stack = [],
      _halted = False,
      _input = [],
      _bypass = M.empty,
      _solution = if auto then precomputed else []
    }

-- given a value, interpret it as either a memory literal or register
interpMemory :: S.Seq Int -> Int -> Int
interpMemory memory' val
  | val < 32768 = val
  | otherwise = S.index memory' val

instance Show VM where
  show VM {_memory, _ptr, _stack, _input, _bypass} =
    unlines $
      zipWith
        (++)
        (map (++ ": ") ["ptr", "registers", "stack", "input", "bypass"])
        [show _ptr, show registers, show _stack, show _input, show _bypass]
    where
      registers = map (S.index _memory) [32768 .. 32775]

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

{- ORMOLU_DISABLE -}

-- parse admin commands
admin :: VM -> Parser (IO VM)
admin vm =
  do
    symbol "state" >> return (do print vm; return vm)
    <|> 
  do
    _ <- symbol "set reg"
    reg <- (+ 32768) <$> natural
    val <- natural
    return $ return (over memory (S.update reg val) vm)
    <|> 
  do
    _ <- symbol "peek"
    start <- natural
    stop <- natural
    let p = assembly True start $ map (S.index $ _memory vm) [start .. stop]  
    return $ do p; return vm
    <|> 
  do
    _ <- symbol "bypass"
    addr <- natural
    action <- many (sat (/= '\n'))
    return $ return (over bypass (M.insert addr action) vm)
    <|> 
  do 
    symbol "halt" >> return (return $ set halted True vm)

{- ORMOLU_ENABLE -}

-- take user input, including admin commands that can mutate the VM
-- either use precomputed or user input, each potentially using an admin command
handleInput :: VM -> IO VM
handleInput vm@(VM {_solution = sol : solution', _input = []}) =
  do
    putStr $ "> " ++ sol
    case parse (admin vm) sol of
      Nothing -> return $ (set solution solution' . set input sol) vm
      Just (io, _) -> io >>= (handleInput . set solution solution')
handleInput vm@(VM {_solution = [], _input = []}) =
  do
    putStr "> "
    hFlush stdout
    action <- (++ "\n") <$> getLine
    case parse (admin vm) action of
      Nothing -> return $ set input action vm
      Just (io, _) -> io >>= handleInput
handleInput vm = return vm

-- an iteration of the virtual machine
step :: VM -> IO VM
step vm =
  if M.member (_ptr vm) (_bypass vm)
    then do
      let action = _bypass vm M.! _ptr vm
      let opcode = toEnum $ S.index (_memory vm) (_ptr vm)
      let io = fst $ fromJust $ parse (admin vm) action
      over ptr (+ width opcode) <$> io
    else do
      let opcode = toEnum $ S.index (_memory vm) (_ptr vm)

      -- input is placed first, in case it changes the VM via an admin command!
      vm'@(VM {_memory, _ptr, _stack, _input}) <- case opcode of
        In -> handleInput vm
        _ -> return vm

      -- this is lazy, cool!
      let a_imm = S.index _memory (_ptr + 1)
      let b_imm = S.index _memory (_ptr + 2)
      let c_imm = S.index _memory (_ptr + 3)
      let a_val = interpMemory _memory a_imm
      let b_val = interpMemory _memory b_imm
      let c_val = interpMemory _memory c_imm

      when
        (opcode == Out)
        (putChar $ toEnum a_val)

      -- this is just for readability
      let mem addr val = over memory (S.update addr val)
      let inc = over ptr (+ width opcode)

      return $
        vm'
          & ( case opcode of
                Halt -> set halted True
                Set -> inc . mem a_imm b_val
                Push -> inc . over stack (a_val :)
                Pop ->
                  let hd : stack' = _stack
                   in inc . mem a_imm hd . set stack stack'
                Eq -> inc . mem a_imm (if b_val == c_val then 1 else 0)
                Gt -> inc . mem a_imm (if b_val > c_val then 1 else 0)
                Jmp -> set ptr a_val
                Jt -> if a_val /= 0 then set ptr b_imm else inc
                Jf -> if a_val == 0 then set ptr b_imm else inc
                Add -> inc . mem a_imm ((b_val + c_val) `mod` 32768)
                Mult -> inc . mem a_imm ((b_val * c_val) `mod` 32768)
                Mod -> inc . mem a_imm (b_val `mod` c_val)
                And -> inc . mem a_imm (b_val .&. c_val)
                Or -> inc . mem a_imm (b_val .|. c_val)
                Not -> inc . mem a_imm (complement b_val `mod` 32768)
                Rmem -> inc . mem a_imm (interpMemory _memory $ S.index _memory b_val)
                Wmem -> inc . mem a_val b_val
                Call -> set ptr a_val . over stack (_ptr + width opcode :)
                Ret ->
                  case _stack of
                    [] -> set halted True
                    hd : stack' -> set ptr hd . set stack stack'
                In ->
                  case _input of
                    hd : tl -> inc . set input tl . mem a_imm (fromEnum hd)
                    [] -> const vm
                Out -> inc
                Noop -> inc
            )

-- iterate until the VM halts
untilHalt :: VM -> IO VM
untilHalt vm@(VM {_halted = True}) = return vm
untilHalt vm = step vm >>= untilHalt

-- a function for printing assembly from a binary

{- ORMOLU_DISABLE -}

assembly :: (Num t, Eq t, Ord t, Integral t, Show t, PrintfArg t) => Bool -> Int -> [t] -> IO ()
assembly _ _ [] = return ()
assembly str_start ptr' (o : xs)
  | o == 19 =
      do
        let a : tl = xs
        -- is this the start of the string?
        when str_start $ putStr (printf "%06d: out \"" ptr')
        -- print the char
        let c :: Char = (toEnum . fromInteger . toInteger) a
        let s = if c == '\n' then "\\n" else [c]
        putStr s
        -- is this the end of the string?
        case tl of
          tl'@(19 : _) -> assembly False (ptr' + 2) tl'
          tl'@(_ : _) -> putStr "\"\n" >> assembly True (ptr' + 2) tl'
          [] -> putStr "\"\n"
  | o <= 21 =
      let opcode :: Opcode = (toEnum . fromInteger . toInteger) o in
      case (width opcode, xs) of
        (1,             tl) -> putStrLn (printf "%06d: %s"          ptr' (show opcode)                     ) >> assembly True (ptr' + 1) tl
        (2, a :         tl) -> putStrLn (printf "%06d: %s %s"       ptr' (show opcode) (md a)              ) >> assembly True (ptr' + 2) tl
        (3, a : b :     tl) -> putStrLn (printf "%06d: %s %s %s"    ptr' (show opcode) (md a) (md b)       ) >> assembly True (ptr' + 3) tl
        (4, a : b : c : tl) -> putStrLn (printf "%06d: %s %s %s %s" ptr' (show opcode) (md a) (md b) (md c)) >> assembly True (ptr' + 4) tl
        _ -> return ()
  | otherwise = putStrLn (printf "%06d: data %s" ptr' (md o)) >> assembly True (ptr' + 1) xs
  where
    md val
      | val < 32768 = show val
      | otherwise = printf "$%d" (val - 32768)

{- ORMOLU_ENABLE -}

-- precomputed solution
precomputed :: [String]
precomputed =
  map
    (++ "\n")
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
      "use teleporter",
      "set reg 7 25734",
      "bypass 5511 set reg 0 6",
      "use teleporter",
      "north",
      "north",
      "north",
      "north",
      "north",
      "north",
      "north",
      "north",
      "north",
      "take orb",
      "north",
      "east",
      "east",
      "north",
      "west",
      "south",
      "east",
      "east",
      "west",
      "north",
      "north",
      "east",
      "vault",
      "take mirror",
      "use mirror",
      "halt",
      "done!"
    ]
