{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad
import Criterion.Main
import Criterion.Types
import Data.ByteString.Internal
import Data.FileEmbed
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import Language.BrainHask
import Pipes
import System.FilePath (takeExtension)

data Program = Program
  { filename :: FilePath,
    optimizationLevel :: OptimizationLevel,
    bfProgram :: ILProgram Int
  }

parseBS :: ByteString -> IO BFProgram
parseBS =
  either (throwIO . userError . show) pure
    . parseBF
    . unpack
    . decodeUtf8With lenientDecode

brainfuckFiles :: [(FilePath, ByteString)]
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
benchMachineIO = (inp', outp')
  where
    inp' = each (c2w <$> "23\n")
    outp' = await >> outp

inp :: Producer Word8 IO ()
outp :: Consumer Word8 IO ()
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
