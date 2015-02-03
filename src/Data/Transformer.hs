{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveFunctor, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Transformer where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Data

data Transformer a b = Transformer { runT :: [a] -> Maybe (b, [a])} deriving (Functor)

type Parser b = Transformer String b

(|>) :: b -> (b -> c) -> c
(|>) = flip ($)

instance Applicative (Transformer a) where
    pure a  = Transformer (\ri -> Just (a,ri))
    p <*> q = Transformer (\cs -> do
        (a, cs')  <- runT p cs
        (b, cs'') <- runT q cs'
        Just (a b, cs''))

instance Alternative (Transformer a) where
    empty   = Transformer (\_ -> Nothing )
    p <|> q = Transformer (\s -> runT p s <|> runT q s )

instance Monad (Transformer a) where
   return     = pure
   p1 >>= fp2 = Transformer $ \cs -> do
        (a, cs') <- runT p1 cs
        runT (fp2 a) cs'

instance MonadPlus (Transformer a) where
    mzero = empty
    mplus = (<|>)

satisfies :: (a -> Bool) -> Transformer a a
satisfies f = do
    op <- item
    if f op then return op else Transformer $ const Nothing

sameConstr :: (Data a, Data b) => a -> b -> Bool
sameConstr a b = toConstr a == toConstr b

match :: (Eq a) => a -> Transformer a a
match a = satisfies (a ==)

item :: Transformer a a
item = Transformer f where
    f (x:xs) = Just (x,xs)
    f []     = Nothing

choice :: [Transformer a b] -> Transformer a b
choice = foldl1 (<|>)

matchConstructor :: (Data a) => a -> Transformer a a
matchConstructor a = satisfies (sameConstr a)

many :: Transformer a b -> Transformer a [b]
many p = many1 p <|> return []

many1 :: Transformer a b -> Transformer a [b]
many1 p = do { a <- p; as <- many p; return (a:as)}

transform :: Transformer a b -> [a] -> [b]
transform _ [] = []
transform t p@(_:ps) = case runT t p of
            Nothing    -> transform t ps
            Just (x,r) -> x : transform t r

mtransform :: [Transformer a a] -> [a] -> [a]
mtransform _ []  = []
mtransform [] vs = vs
mtransform (t:ts) p@(p1:ps) = case runT t p of
            Nothing    -> mtransform ts $ p1 : mtransform [t] ps
            Just (x,r) -> mtransform ts $  x : mtransform [t] r
