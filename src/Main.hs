{-# LANGUAGE LambdaCase #-}

module Main where

import Brainhask.Interpreter
import Brainhask.Parser
import Brainhask.Optimizer
import Options.Applicative
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

processOptions :: Options -> IO ()
processOptions (Options f o a) = do
    fileExists <- doesFileExist f
    if (not fileExists) then putStrLn (f ++ " does not exist.") >> return () else
        readFile f >>= return . parseBF >>= \case
            Left errorMsg -> print errorMsg
            Right program -> printASTorRun a $ mayOpt o program
    where
        mayOpt True        = optimize
        mayOpt _           = id
        printASTorRun True = print
        printASTorRun _    = interpretBF

main :: IO ()
main = do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    execParser opts >>= processOptions
    putStrLn ""
    where
        opts = info (helper <*> options) ( fullDesc <> progDesc "Brainfuck interpreter")
