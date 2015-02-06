{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.BrainHask.Types where

import           Data.Data
import           Data.Foldable
import           Data.Monoid

data Op a
  = NoOp
  | Move a
  | Add a
  | Set a
  | Loop [Op a]
  | Get a
  | Put a
  deriving (Show, Eq, Functor, Data, Typeable)

type BFProgram a = [Op a]

extractOp :: (Monoid a) => Op a -> a
extractOp (NoOp   ) = mempty
extractOp (Move x ) = x
extractOp (Add x  ) = x
extractOp (Put x  ) = x
extractOp (Get x  ) = x
extractOp (Set x  ) = x
extractOp (Loop x ) = extract x

extract :: (Monoid b) => BFProgram b -> b
extract = foldMap extractOp
