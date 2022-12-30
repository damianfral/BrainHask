{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.BrainHask.CLI where

import Data.ByteString.Internal
import Data.Either.Combinators
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics
import Language.BrainHask
import Language.BrainHask.Interpreter
import Options.Applicative (ReadM)
import Options.Generic
import Pipes
import System.Directory
import System.IO
import Text.Pretty.Simple (pPrint)

newtype File = File FilePath
  deriving (Generic, Typeable)

instance ParseRecord File

instance ParseFields File

instance ParseField File where
  readField = File <$> readField

data Options w = Options
  { input ::
      w
        ::: File
        <?> "brainfuck file"
        <#> "i",
    optimize ::
      w
        ::: OptimizationLevel
        <?> "optimization level (0|1|2)"
        <#> "o"
        <!> "3",
    ast ::
      w
        ::: Bool
        <?> "print the abstract syntax tree"
        <#> "a"
  }
  deriving (Generic, Typeable)

instance ParseRecord OptimizationLevel

instance ParseFields OptimizationLevel

instance ParseField OptimizationLevel where
  readField =
    readField >>= parseStr
    where
      parseStr :: Int -> ReadM OptimizationLevel
      parseStr 1 = pure O1
      parseStr 2 = pure O2
      parseStr _ = pure O0

instance ParseRecord (Options Wrapped)

readInput :: FilePath -> IO (Either String String)
readInput fn =
  doesFileExist fn >>= \b ->
    if b
      then Right <$> readFile fn
      else pure $ Left $ fn ++ " does not exist"

runOptions :: Options Unwrapped -> (Producer Word8 IO (), Consumer Word8 IO ()) -> IO ()
runOptions (Options (File input) o ast) (inp, outp) = do
  string <- readInput input
  let program = string >>= mapLeft show . parseBF
  go program
  where
    go :: Either String BFProgram -> IO ()
    go (Left errorMsg) = runEffect $ each (map c2w errorMsg) >-> outp
    go (Right program)
      | ast = pPrint $ compile program
      | otherwise = void $ interpretBF inp outp $ compile program
      where
        compile = Language.BrainHask.optimize o . preprocess

runCLI = do
  options <- unwrapRecord "brainhask - a brainfuck interpreter"
  runOptions options machineIO
