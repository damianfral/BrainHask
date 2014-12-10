{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Brainhask.Optimizer where

import Brainhask.Types
import Control.Applicative hiding (many)
import Control.Monad
import Data.Data
import Data.Functor
import Data.List
import Data.Transformer
import Data.Typeable

type BFTransformer = Transformer (Op Int) (Op Int)

reduceModifies :: BFTransformer
reduceModifies = do
    ms <- many1 $ matchConstructor (Modify 1)
    return $ Modify $ sum $ map (\(Modify x) -> x) ms

reduceLoops :: BFTransformer
reduceLoops = satisfies (== (Loop [Modify (-1)]))  *> pure (Set 0)

reduceMoves :: BFTransformer
reduceMoves = do
    ms <- many1 $ matchConstructor (Move 1)
    return $ Move $ sum $ map (\(Move x) -> x) ms

removeModifies0, removeMoves0  :: BFTransformer

removeModifies0 = do
    many1 $ match (Modify 0)
    return NoOp

removeMoves0    = do
    many1 $ match (Move 0)
    return NoOp

optimizeLoops :: [BFTransformer] -> BFTransformer
optimizeLoops opts = do
    (Loop ops) <- matchConstructor (Loop [])
    return $ Loop $ mtransform opts ops

allOpt = [reduceModifies, reduceMoves, optimizeLoops allOpt, removeModifies0, removeMoves0, reduceLoops]

optimize = filter (/= NoOp) . mtransform allOpt
