{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveFunctor, DeriveDataTypeable #-}

module Main where

import Language.BrainHask.Interpreter
import Language.BrainHask.Optimizer
import Language.BrainHask.Parser
import Control.Monad
import Data.Either.Combinators
import Options.Applicative
import System.Directory
import System.IO
import Control.Concurrent
import Pipes

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

processOptions :: Options -> IO ()
processOptions (Options fn o a) = go <$> readInput fn >>= writeOutput
    where
        go Nothing   = Left $ fn ++ " does not exist"
        go (Just fc) = mapLeft show $ parseBF fc
        writeOutput ( Left errorMsg ) = putStrLn errorMsg
        writeOutput ( Right program ) | o && a      = print       $ optimize program
                                      | o           = interpretBF $ optimize program
                                      | a           = print       $ program
                                      | otherwise   = interpretBF $ program

main :: IO ()
main = do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    execParser opts >>= processOptions
    putStrLn ""
        where
            opts = info ( helper <*> options )
                        ( fullDesc <> progDesc "Brainfuck interpreter" )
