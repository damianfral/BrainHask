module Data.Tape
    ( Tape(..) , tapeOf      , moveCursor
    , getCursor, updateCursor, replaceCursor
    , getIndex , updateIndex , replaceIndex ) where

data Tape a = Tape {_left :: [a], _getCursor :: !a, _right :: [a]}

tapeOf :: a -> Tape a
tapeOf x = Tape (repeat x) x (repeat x)

getCursor = _getCursor

moveCursor :: Int -> Tape a -> Tape a
moveCursor   0  t = t
moveCursor   1  (Tape (ls) a (r:rs)) = Tape (a:ls) r rs
moveCursor (-1) (Tape (l:ls) a (rs)) = Tape ls l (a:rs)
moveCursor  n t@(Tape ls c rs) = foldr1 (.) (replicate (abs n) (moveCursor $ signum n))  t

replaceCursor :: a -> Tape a -> Tape a
replaceCursor a t = t {_getCursor = a}

updateCursor :: (a -> a) -> Tape a -> Tape a
updateCursor f (Tape l a r) = Tape l (f a) r

getIndex :: Int -> Tape a -> a
getIndex p = getCursor . (moveCursor p)

replaceIndex :: Int -> a -> Tape a -> Tape a
replaceIndex p a = moveCursor (-p) . replaceCursor a . moveCursor p

updateIndex :: Int -> (a -> a) -> Tape a -> Tape a
updateIndex p f = moveCursor (-p) . updateCursor f . moveCursor p
