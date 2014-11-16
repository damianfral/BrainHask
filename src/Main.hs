{-# LANGUAGE LambdaCase #-}

module Main where

import Brainhask.Interpreter
import Brainhask.Parser
import Control.Applicative
import Control.Monad
import System.Environment
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    getArgs >>= mapM_ ( readFile >=> return . parseBF >=> \case
        Left errorMsg -> print errorMsg
        Right program -> interpretBF program )
    putStrLn ""
