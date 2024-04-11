module Main where

import Control.Monad (when)
import Options.Applicative
import Synacor

data Action = Convert | Run deriving (Eq)

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
    if actionOpt == Run
      then do
        putStrLn ""
        let vm = fromBinary autoOpt bin
        vm' <- untilHalt vm
        when printOpt (print vm')
      else assembly 0 bin
