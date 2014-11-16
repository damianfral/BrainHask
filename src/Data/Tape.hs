module Data.Tape
    ( Tape()
    , _left, _cursor, _right, tapeOf
    , moveLeft, moveRight, modifyCursor, replaceCursor) where

data Tape a = Tape {_left :: [a], _cursor :: a, _right :: [a]}

tapeOf :: a -> Tape a
tapeOf x = Tape (repeat x) x (repeat x)

moveLeft, moveRight :: Tape a -> Tape a
moveLeft  (Tape lefts cc (nc:rights)) = Tape (cc:lefts) nc rights
moveRight (Tape (nc:lefts) cc rights) = Tape lefts nc (cc:rights)

modifyCursor :: (a -> a) -> Tape a -> Tape a
modifyCursor f (Tape ls x rs) = Tape ls (f x) rs

replaceCursor :: a -> Tape a -> Tape a
replaceCursor = modifyCursor . const
