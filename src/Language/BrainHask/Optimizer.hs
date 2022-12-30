{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.BrainHask.Optimizer (optimize, OptimizationLevel (..)) where

import Control.Applicative
import Data.Functor
import Data.List
import Data.Transformer
import GHC.Generics (Generic)
import Language.BrainHask.Types

-------------------------------------------------------------------------------

data OptimizationLevel = O0 | O1 | O2 | O3 deriving (Eq, Show, Enum, Generic)

set0Optimization (ILLoop [ILAdd _]) = ILSet 0
set0Optimization (ILLoop xs) = ILLoop $ map set0Optimization xs
set0Optimization l = l

multOptimization l@(ILLoop [ILMove m1, ILAdd a1, ILMove m2, ILAdd (-1)])
  | m1 + m2 == 0 = ILBlock [ILMove m1, ILAddMult (-m1) a1, ILMove (-m1), ILSet 0]
  | otherwise = l
multOptimization l@(ILLoop [ILAdd (-1), ILMove m1, ILAdd a1, ILMove m2]) =
  multOptimization (ILLoop [ILMove m1, ILAdd a1, ILMove m2, ILAdd (-1)])
multOptimization (ILLoop xs) = ILLoop $ map multOptimization xs
multOptimization l = l

addToOptimization l@(ILLoop [ILMove m1, ILAdd x, ILMove m2, ILAdd (-1)])
  | m1 + m2 == 0 && x > 0 = ILBlock $ replicate x (ILAddTo m1) <> [ILSet 0]
  | otherwise = l
addToOptimization l@(ILLoop [ILAdd (-1), ILMove m1, ILAdd 1, ILMove m2]) =
  addToOptimization $ ILLoop [ILMove m1, ILAdd 1, ILMove m2, ILAdd (-1)]
addToOptimization (ILLoop xs) = ILLoop $ map addToOptimization xs
addToOptimization l = l

doubleAddToOptimization
  l@( ILLoop
        [ ILMove m1,
          ILAdd x,
          ILMove m2,
          ILAdd y,
          ILMove m3,
          ILAdd (-1)
          ]
      ) =
    if m1 + m2 == (-m3) && x > 0 && y > 0
      then
        ILBlock $
          mconcat
            [ replicate x $ ILAddTo m1,
              replicate y $ ILAddTo m2,
              [ILSet 0]
            ]
      else l
doubleAddToOptimization (ILLoop xs) = ILLoop $ map doubleAddToOptimization xs
doubleAddToOptimization l = l

transform :: Int -> ILProgram Int -> ILProgram Int
transform n = foldr1 (.) optList
  where
    optList = intersperse expandBlocks $ fmap <$> take n optimizations

optimizations =
  [ set0Optimization,
    multOptimization,
    addToOptimization,
    doubleAddToOptimization
  ]

optimize :: OptimizationLevel -> ILProgram Int -> ILProgram Int
optimize O0 = filter (/= ILNoOp)
optimize O1 = filter (/= ILNoOp) . transform 1
optimize O2 = filter (/= ILNoOp) . transform 3
optimize O3 = filter (/= ILNoOp) . transform (length optimizations)
