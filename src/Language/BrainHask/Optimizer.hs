module Language.BrainHask.Optimizer (optimize) where

import Language.BrainHask.Types
import Control.Applicative hiding (many)
import Data.Monoid
import Data.Transformer

ffmap  = fmap . fmap
sumOps = getSum . extract . ffmap Sum

type BFTransformer = Transformer (Op Int) (Op Int)

reduceModifies :: BFTransformer
reduceModifies = Add . sumOps <$> many1 (matchConstructor  $ Add 0)

reduceLoops :: BFTransformer
reduceLoops = satisfies (== Loop [Add (-1)]) *> pure (Set 0)

reduceMoves :: BFTransformer
reduceMoves = Move . sumOps <$> many1 (matchConstructor  $ Move 0)

removeModifies0, removeMoves0  :: BFTransformer
removeModifies0 = many1 (match $ Add  0) *> pure NoOp
removeMoves0    = many1 (match $ Move 0) *> pure NoOp

optimizeLoops :: [BFTransformer] -> BFTransformer
optimizeLoops opts = do
    (Loop ops) <- matchConstructor (Loop [])
    return $ Loop $ mtransform opts ops

allOpt :: [BFTransformer]
allOpt = [reduceModifies, reduceMoves, optimizeLoops allOpt, removeModifies0, removeMoves0, reduceLoops]

optimize :: BFProgram Int -> BFProgram Int
optimize = filter (/= NoOp) . mtransform allOpt
