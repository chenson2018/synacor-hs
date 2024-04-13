module Main where

import Control.Monad (when)
import Control.Monad.Trans.Maybe (runMaybeT)
import Options.Applicative
import Synacor

data Action
  = Convert
  | Run

data Args = Args
  { path :: String,
    printOpt :: Bool,
    autoOpt :: Bool,
    actionOpt :: Action
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
    <*> ( flag' Convert (long "convert" <> help "convert from binary")
            <|> flag' Run (long "run" <> help "run binary")
        )

opts :: ParserInfo Args
opts = info (args <**> helper) (fullDesc <> progDesc "Run or analyze a Synacor program")

main :: IO ()
main =
  do
    Args {path, printOpt, autoOpt, actionOpt} <- execParser opts
    bin <- readBinary path
    case actionOpt of
      Run ->
        do
          let vm = fromBinary autoOpt bin
          vm' <- runMaybeT (bindUntil _halted step vm)
          when printOpt (print vm')
      Convert ->
        assembly True 0 bin
