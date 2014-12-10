{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Brainhask.Optimizer where

import Brainhask.Types
import Control.Applicative hiding (many)
import Control.Monad
import Data.Data
import Data.Functor
import Data.Monoid
import Data.List
import Data.Transformer
import Data.Typeable

ffmap  = fmap . fmap
sumOps = getSum . extract . ffmap Sum

type BFTransformer = Transformer (Op Int) (Op Int)

reduceModifies :: BFTransformer
reduceModifies = Modify . sumOps <$> (many1 $ matchConstructor (Modify 0))

reduceLoops :: BFTransformer
reduceLoops = satisfies (== (Loop [Modify (-1)])) *> pure (Set 0)

reduceMoves :: BFTransformer
reduceMoves = Move . sumOps <$> (many1 $ matchConstructor (Move 0))

removeModifies0, removeMoves0  :: BFTransformer
removeModifies0 = (many1 $ match (Modify 0)) *> pure NoOp
removeMoves0    = (many1 $ match (Move 0))   *> pure NoOp

optimizeLoops :: [BFTransformer] -> BFTransformer
optimizeLoops opts = do
    (Loop ops) <- matchConstructor (Loop [])
    return $ Loop $ mtransform opts ops

allOpt = [reduceModifies, reduceMoves, optimizeLoops allOpt, removeModifies0, removeMoves0, reduceLoops]
optimize = filter (/= NoOp) . mtransform allOpt
