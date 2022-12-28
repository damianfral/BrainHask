{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Internal
import Data.Either.Combinators
import Data.Tape
import Language.BrainHask
import Options.Applicative
import System.Directory
import System.IO

data Options = Options
  { _input :: String,
    _optimize :: Bool,
    _ast :: Bool
  }

options :: Parser Options
options =
  Options
    <$> argument str (metavar "FILENAME")
    <*> switch (short 'o' <> long "optimize" <> help "Try to optimize")
    <*> switch (short 'a' <> long "ast" <> help "Print the AST")

readInput :: FilePath -> IO (Either String String)
readInput fn =
  doesFileExist fn >>= \b ->
    if b
      then Right <$> readFile fn
      else return $ Left $ fn ++ " does not exist"

initMachine :: IO Machine
initMachine = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  return $ Machine machineMemory machineIO
  where
    machineMemory = tapeOf 0
    machineIO = MachineIO (putStr "\n> " >> c2w <$> getChar) (putStr . map w2c)

main :: IO ()
main = do
  (Options input o a) <- execParser opts
  machine <- initMachine
  string <- readInput input
  let program = string >>= mapLeft show . parseBF
  go o a machine program
  where
    opts =
      info
        (helper <*> options)
        (fullDesc <> progDesc "Brainfuck interpreter")
    go _ _ (Machine _ (MachineIO _ wf)) (Left errorMsg) = wf $ map c2w errorMsg
    go o a machine@(Machine _ (MachineIO _ wf)) (Right program)
      | o && a = wf . map c2w . show $ optimize program
      | o = interpretBF machine $ optimize program
      | a = wf . map c2w . show $ program
      | otherwise = interpretBF machine program
