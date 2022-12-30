{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Trans.Except
import Criterion.Main
import Criterion.Types
import Data.Bifunctor (bimap)
import Data.ByteString.Internal
import Data.FileEmbed
import Data.Functor
import Data.Tape
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import Language.BrainHask
import Language.BrainHask.CLI hiding (fakeMachineIO, optimize)
import Language.BrainHask.Optimizer
import Pipes
import Pipes.Lift
import System.FilePath (takeExtension)
import System.IO

data Program = Program
  { filename :: FilePath,
    optimizationLevel :: OptimizationLevel,
    bfProgram :: ILProgram Int
  }

parseBS =
  either (throwIO . userError . show) pure
    . parseBF
    . unpack
    . decodeUtf8With lenientDecode

brainfuckFiles = filter isBrainfuckFile files
  where
    files = $(makeRelativeToProject "brainfucks/" >>= embedDir)
    isBrainfuckFile = (==) ".bf" . takeExtension . fst

loadPrograms :: (FilePath, ByteString) -> IO [Program]
loadPrograms (filename, src) = do
  program <- parseBS src
  pure $
    [ Program filename optimizationLevel optimizedProgram
      | optimizationLevel <- [O0 .. O3],
        optimizedProgram <- [optimize optimizationLevel $ preprocess program]
    ]

main :: IO ()
main = do
  programs <- concat <$> mapM loadPrograms brainfuckFiles
  defaultMainWith config $ benchmarkProgram <$> programs
  where
    config = defaultConfig {resamples = 5}

benchMachineIO :: (Producer Word8 IO (), Consumer Word8 IO ())
benchMachineIO = (inp, outp)
  where
    inp = each (c2w <$> "23\n")
    outp = await >> outp

(inp, outp) = benchMachineIO

benchmarkProgram :: Program -> Benchmark
benchmarkProgram Program {..} =
  bgroup
    filename
    [ bench (show optimizationLevel) $
        whnfIO $
          void $
            interpretBF inp outp bfProgram
    ]
