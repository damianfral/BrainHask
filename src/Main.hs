{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveFunctor, DeriveDataTypeable #-}

module Main where

import Data.ByteString.Internal
import Data.Either.Combinators
import Data.Tape
import Language.BrainHask.Interpreter
import Language.BrainHask.Optimizer
import Language.BrainHask.Parser
import Options.Applicative
import Pipes
import System.Directory
import System.IO

data Options = Options
    { _file     :: String
    , _optimize :: Bool
    , _ast      :: Bool }

options :: Parser Options
options = Options
    <$> argument str (metavar "FILENAME")
    <*> switch (short 'o' <> long "optimize" <> help "Try to optimize")
    <*> switch (short 'a' <> long "ast"      <> help "Print the AST")

readInput :: FilePath -> IO (Maybe String)
readInput fn = doesFileExist fn >>= \b ->
    if b then Just <$> readFile fn
    else      return Nothing

writeOutput _ _ ( Left errorMsg ) = putStrLn errorMsg
writeOutput o a ( Right program )
        | o && a      = print       optimize program
        | o           = interpretBF machine $ optimize program
        | a           = print       program
        | otherwise   = interpretBF machine $ program
            where
                machineMemory = tapeOf 0
                machineIO     = MachineIO (putStr "\n> " >> (c2w <$> getChar)) (putStr . map w2c)
                machine       = Machine machineMemory machineIO

processOptions :: Options -> IO ()
processOptions (Options fn o a) = go <$> readInput fn >>= writeOutput o a
    where
        go Nothing   = Left $ fn ++ " does not exist"
        go (Just fc) = mapLeft show $ parseBF fc

main :: IO ()
main = do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    execParser opts >>= processOptions
    putStrLn ""
        where
            opts = info ( helper <*> options )
                        ( fullDesc <> progDesc "Brainfuck interpreter" )
