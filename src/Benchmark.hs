{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import           Criterion.Main
import           Criterion.Types
import           Data.ByteString.Internal

import           Language.BrainHask
import           Language.BrainHask.Optimizer

import           Control.Monad
import           Data.Functor
import           Data.Tape
import           Data.Word                          (Word8)
import           Helper                             hiding (machineIO)
import           Pipes
import           Pipes.Lift
import           System.IO

machineIO ::  (Producer Word8 IO (), Consumer Word8 IO ())
machineIO = (inp, outp)
    where
        inp   = each (c2w <$> "23\n")
        outp  = do
            x <- await
            lift $ putStr $ replicate 1 $ w2c x
            lift $ hFlush stdout
            outp

main :: IO ()
main = defaultMainWith benchConfig  benchmarks

benchmarks :: [Benchmark]
benchmarks = [ bgroup "primes"
                 [ bench ("Optimization " ++ show level) $ whnfIO $ runOptions (Options "brainfucks/primes.bf" level False) machineIO | level <- [O0, O1, O2] ]
             ]

benchConfig :: Config
benchConfig = defaultConfig { confInterval = 0.1, forceGC = True }
