{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GADTs #-}

module Brainhask.Types where

import Data.Data
import Data.Typeable

data MoveT
data ModifyT
data InOutT
data EmptyT
data LoopT

data Op  a  = NoOp
            | Move   !a
            | Modify !a
            | Put    !a
            | Get    !a
            | Set    !a
            | Loop   ![Op a] deriving (Show, Eq, Functor, Data, Typeable)


type Program a = [Op a]
