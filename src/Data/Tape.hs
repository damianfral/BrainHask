{-# LANGUAGE BangPatterns #-}

module Data.Tape
    ( Tape()
    , _left, _cursor, _right, tapeOf
    , moveLeft, moveRight
    , modifyCursor, replaceCursor) where

data Tape a = Tape {_left :: ![a], _cursor :: !a, _right :: ![a]}

tapeOf :: a -> Tape a
tapeOf x = Tape (repeat x) x (repeat x)

moveLeft, moveRight :: Int -> Tape a -> Tape a

moveLeft  n t@(Tape ls c rs)  | n < 0  = moveRight (-n) t
                              | n == 0 = t
                              | n > 0  = Tape ls' c' rs'
                                    where !(tmp, ls') = splitAt n ls
                                          !(c':rs') = (reverse tmp) ++ [c] ++ rs

moveRight n t@(Tape ls c rs)  | n < 0  = moveLeft (-n) t
                              | n == 0 = t
                              | n > 0  = Tape ls' c' rs'
                                    where !(tmp, rs') = splitAt n rs
                                          !(c':ls') = (reverse tmp) ++ [c] ++ ls

modifyCursor :: (a -> a) -> Tape a -> Tape a
modifyCursor f (Tape ls x rs) = Tape ls (f x) rs

replaceCursor :: a -> Tape a -> Tape a
replaceCursor = modifyCursor . const
