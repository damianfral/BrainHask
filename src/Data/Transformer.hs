{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Transformer where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Data

newtype Transformer a b = Transformer
  {runT :: [a] -> Maybe (b, [a])}
  deriving (Functor)

type Parser b = Transformer String b

(|>) :: b -> (b -> c) -> c
(|>) = flip ($)

instance Applicative (Transformer a) where
  pure a = Transformer (\ri -> Just (a, ri))
  p <*> q =
    Transformer
      ( \cs -> do
          (a, cs') <- runT p cs
          (b, cs'') <- runT q cs'
          Just (a b, cs'')
      )

instance Alternative (Transformer a) where
  empty = Transformer (const Nothing)
  p <|> q = Transformer (\s -> runT p s <|> runT q s)

instance Monad (Transformer a) where
  p1 >>= fp2 = Transformer $ \cs -> do
    (a, cs') <- runT p1 cs
    runT (fp2 a) cs'

instance MonadPlus (Transformer a) where
  mzero = empty
  mplus = (<|>)

instance MonadFail (Transformer a) where
  fail msg = Transformer $ const Nothing

satisfies :: (a -> Bool) -> Transformer a a
satisfies f = do
  op <- item
  if f op then pure op else Transformer $ const Nothing

sameConstr :: (Data a, Data b) => a -> b -> Bool
sameConstr a b = toConstr a == toConstr b

match :: (Eq a) => a -> Transformer a a
match a = satisfies (a ==)

item :: Transformer a a
item = Transformer f
  where
    f (x : xs) = Just (x, xs)
    f [] = Nothing

choice :: [Transformer a b] -> Transformer a b
choice = foldl1 (<|>)

matchConstructor :: (Data a) => a -> Transformer a a
matchConstructor a = satisfies (sameConstr a)

many :: Transformer a b -> Transformer a [b]
many p = many1 p <|> pure []

many1 :: Transformer a b -> Transformer a [b]
many1 p = (:) <$> p <*> many p

howMany1 :: Transformer a b -> Transformer a Int
howMany1 p = length <$> many1 p

transformOrOmit :: Transformer a b -> [a] -> [b]
transformOrOmit _ [] = []
transformOrOmit t inp@(_ : inps) = case runT t inp of
  Nothing -> transformOrOmit t inps
  Just (x, r) -> x : transformOrOmit t r

endotransformOrPassthrough :: [Transformer a a] -> [a] -> [a]
endotransformOrPassthrough _ [] = []
endotransformOrPassthrough [] vs = vs
endotransformOrPassthrough ps inp@(i : inps) = case runT (choice ps) inp of
  Nothing -> i : endotransformOrPassthrough ps inps
  Just (x, r) -> x : endotransformOrPassthrough ps r
