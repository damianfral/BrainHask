{-# LANGUAGE DeriveDataTypeable #-}

module Language.BrainHask.Types where

import Data.Data

data BFOp
  = BFNoOp
  | BFMoveRight
  | BFMoveLeft
  | BFIncrease
  | BFDecrease
  | BFRead
  | BFWrite
  | BFLoop [BFOp]
  deriving (Show, Eq, Data, Typeable)

type BFProgram = [BFOp]

data ILOp indexDelta value
  = ILNoOp
  | ILMove value
  | ILAdd value
  | ILGet indexDelta
  | ILSet value
  | ILRead
  | ILWrite value
  | ILLoop [ILOp indexDelta value]
  | ILSingleOpLoop indexDelta (ILOp indexDelta value)
  | ILBlock [ILOp indexDelta value]
  | ILAddMult indexDelta value
  | ILMod indexDelta indexDelta indexDelta
  | ILAddTo indexDelta
  deriving (Show, Eq, Data, Typeable)

expandBlock :: ILOp a b -> [ILOp a b]
expandBlock (ILBlock xs) = xs
expandBlock x = [x]

expandBlocks :: [ILOp a b] -> [ILOp a b]
expandBlocks = concatMap expandBlock

type ILProgram a = [ILOp Int a]
