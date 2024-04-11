module Main where

import Control.Monad (when)
import Options.Applicative
import Synacor

data Args = Args
  { path :: String,
    printOpt :: Bool,
    autoOpt :: Bool
  }

args :: Parser Args
args =
  Args
    <$> strOption
      ( long "path"
          <> metavar "PATH"
          <> help "file path to read"
      )
    <*> switch
      ( long "print"
          <> help "print state on halt"
      )
    <*> switch
      ( long "auto"
          <> help "use precomputed solution"
      )

opts :: ParserInfo Args
opts = info (args <**> helper) (fullDesc <> progDesc "Run or analyze a Synacor program")

main :: IO ()
main =
  do
    putStrLn ""
    Args {path, printOpt, autoOpt} <- execParser opts
    bin <- readBinary path
    let vm = fromBinary autoOpt bin
    vm' <- untilHalt vm
    when printOpt (print vm')
