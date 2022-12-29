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

data Options w = Options
  { input :: w ::: String <?> "brainfuck file",
    optimize :: w ::: Maybe OptimizationLevel <?> "optimization level (0|1|2)",
    ast :: w ::: Bool <?> "print the abstract syntax tree"
  }
  deriving (Generic, Typeable)

instance ParseField OptimizationLevel where
  readField =
    readField >>= parseStr
    where
      parseStr :: Int -> ReadM OptimizationLevel
      parseStr 1 = return O1
      parseStr 2 = return O2
      parseStr _ = return O0

instance ParseRecord (Options Wrapped)

readInput :: FilePath -> IO (Either String String)
readInput fn =
  doesFileExist fn >>= \b ->
    if b
      then Right <$> readFile fn
      else return $ Left $ fn ++ " does not exist"

runOptions :: Options Unwrapped -> (Producer Word8 IO (), Consumer Word8 IO ()) -> IO ()
runOptions (Options input o ast) (inp, outp) = do
  string <- readInput input
  let program = string >>= mapLeft show . parseBF
  go program
  where
    go :: Either String BFProgram -> IO ()
    go (Left errorMsg) = runEffect $ each (map c2w errorMsg) >-> outp
    go (Right program)
      | ast = runEffect $ each (map c2w . show $ compile program) >-> outp
      | otherwise = void $ interpretBF inp outp $ compile program
      where
        compile = Language.BrainHask.optimize (fromMaybe O0 o) . preprocess

runCLI = do
  options <- unwrapRecord "brainhask - a brainfuck interpreter"
  runOptions options machineIO
