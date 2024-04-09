module Main where

import Control.Monad (when)
import Options.Applicative
import Synacor

data Args = Args
  { path :: String,
    printOpt :: Bool
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
          <> help "print source code"
      )

opts :: ParserInfo Args
opts = info (args <**> helper) (fullDesc <> progDesc "Run or analyze a Synacor program")

main :: IO ()
main =
  do
    Args {path, printOpt} <- execParser opts
    bin <- readBinary path
    when printOpt (print bin)
