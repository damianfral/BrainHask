{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Trans.Except
import Criterion.Main
import Criterion.Types
import Data.ByteString.Internal
import Data.Either.Combinators (mapLeft)
import Data.Functor
import Data.Tape
import Data.Word (Word8)
import Helper hiding (machineIO, optimize)
import Language.BrainHask
import Language.BrainHask.Optimizer
import Pipes
import Pipes.Lift
import System.IO

data Program = Program
  { filename :: FilePath,
    optimizationLevel :: OptimizationLevel,
    bfProgram :: ILProgram Int
  }

readAndParse str =
  ExceptT (readInput str) >>= ExceptT . pure . mapLeft show . parseBF

brainfuckFiles =
  mappend "brainfucks/"
    <$> [ "primes.bf",
          "triangle.bf"
        ]

loadPrograms :: FilePath -> IO [Program]
loadPrograms filename = do
  runExceptT (readAndParse filename) >>= \case
    Left e -> throwIO $ userError "error parsing files"
    Right program ->
      pure $
        [ Program filename optimizationLevel optimizedProgram
          | optimizationLevel <- [O0, O1, O2],
            optimizedProgram <- [optimize optimizationLevel $ preprocess program]
        ]

main :: IO ()
main = do
  programs <- concat <$> mapM loadPrograms brainfuckFiles
  defaultMainWith defaultConfig {resamples = 3} $ benchmarkOptimizations <$> programs

machineIO :: (Producer Word8 IO (), Consumer Word8 IO ())
machineIO = (inp, outp)
  where
    inp = each (c2w <$> "17\n")
    outp = do
      x <- await
      outp

(inp, outp) = machineIO

benchmarkOptimizations :: Program -> Benchmark
benchmarkOptimizations Program {..} =
  bgroup
    filename
    [ bench (show optimizationLevel) $
        whnfIO $
          void $
            interpretBF inp outp bfProgram
    ]
