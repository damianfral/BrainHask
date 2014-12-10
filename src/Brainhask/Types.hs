{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}

module Brainhask.Types where

import Data.Data
import Data.Monoid

data Op  a  = NoOp
            | Move   !a
            | Modify !a
            | Put    !a
            | Get    !a
            | Set    !a
            | Loop   ![Op a] deriving (Show, Eq, Functor, Data, Typeable)

type Program a = [Op a]

extract :: Monoid a => Program a -> a
extract []                 = mempty
extract ((NoOp     ) : xs) = mempty <> extract xs
extract ((Move x   ) : xs) = x <> extract xs
extract ((Modify x ) : xs) = x <> extract xs
extract ((Put x    ) : xs) = x <> extract xs
extract ((Get x    ) : xs) = x <> extract xs
extract ((Set x    ) : xs) = x <> extract xs
extract ((Loop x   ) : xs) = (extract x) <> extract xs
