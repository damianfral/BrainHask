{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveFunctor, DeriveDataTypeable #-}

module Language.BrainHask.Types where

import Data.Data
import Data.List
import Data.Monoid

data OpType = IOAction | TapeAction
data Op a = NoOp | Move a | Add a | Set a | Loop [Op a] | Get a | Put a deriving (Show, Eq, Functor, Data, Typeable)

opType :: Op a -> OpType
opType (Get _) = IOAction
opType (Put _) = IOAction
opType _       = TapeAction

type BFProgram a = [Op a]

extract :: (Monoid b) => BFProgram b -> b
extract []              = mempty
extract (NoOp     : xs) = extract xs
extract (Move x   : xs) = x <> extract xs
extract (Add x    : xs) = x <> extract xs
extract (Put x    : xs) = x <> extract xs
extract (Get x    : xs) = x <> extract xs
extract (Set x    : xs) = x <> extract xs
extract (Loop x   : xs) = extract x <> extract xs
