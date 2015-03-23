{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Helper
import           Options.Applicative

#ifdef __GHCJS__

main :: IO ()
main = do
    select "#run"   >>= flip onClick runClick
    select "#ast"   >>= flip onClick astClick
    select "#clear" >>= flip onClick (select "#output" >>= flip setVal "")

#else

main :: IO ()
main = do
    options <- execParser parser
    runOptions options machineIO
    where parser = info ( helper <*> options )
                        ( fullDesc <> progDesc "Brainfuck interpreter" )

#endif
