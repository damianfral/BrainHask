{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.BrainHask.Interpreter
  ( MachineMemory,
    MachineM,
    interpretBF,
  )
where

import Control.Applicative
import Control.Monad (replicateM_, when, (>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString.Internal
import Data.Tape
import Data.Word (Word8)
import Language.BrainHask.Types
import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P

type MachineMemory = Tape Word8

type MachineM r = forall m. (Monad m) => Pipe Word8 Word8 (StateT MachineMemory m) r

printOp :: ILOp Int Int -> MachineM (ILOp Int Int)
printOp x = do
  yieldOp x
  yield $ c2w '\n'
  return x
  where
    yieldOp (ILLoop x) = mapM_ (yield . c2w) ("Loop" :: String)
    yieldOp x = mapM_ (yield . c2w) (Prelude.show x)

interpretOpIO :: ILOp Int Int -> MachineM (ILOp Int Int)
interpretOpIO ILRead = do
  ILSet . fromIntegral <$> await
interpretOpIO (ILWrite n)
  | n <= 0 = return ILNoOp
  | otherwise = do
      c <- getCursor <$> lift get
      replicateM_ n $ yield c
      -- mapM_ (yield . c2w ) "WriteOP"
      return ILNoOp
interpretOpIO c = return c

interpretTapeOp :: ILOp Int Int -> MachineM ()
interpretTapeOp ILNoOp = return ()
interpretTapeOp (ILMove n) = lift $ modify $! moveCursor n
interpretTapeOp (ILAdd n) = lift $ modify $! updateCursor $ (+) (fromIntegral n)
interpretTapeOp (ILSet n) = lift $ modify $! replaceCursor $ fromIntegral n
interpretTapeOp (ILAddMult i n) = do
  lift $ modify (\v -> updateCursor (\x -> fromIntegral x + getIndex i v * fromIntegral n) v)
interpretTapeOp (ILBlock ops) = interpret ops
interpretTapeOp (ILLoop []) = return ()
interpretTapeOp (ILSingleOpLoop i a) = do
  c <- getCursor . moveCursor i <$> lift get
  replicateM_ (fromIntegral c) $ interpret [a]
interpretTapeOp (ILLoop ops) = do
  c <- getCursor <$> lift get
  when (c /= 0) $! interpret (ops ++ [ILLoop ops])
interpretTapeOp (ILAddTo n) = do
  c <- getCursor <$> lift get
  mapM_ interpretTapeOp [ILMove n, ILAdd (fromIntegral c), ILMove (-n)]
interpretTapeOp x = return ()

interpret :: ILProgram Int -> MachineM ()
interpret = mapM_ $ interpretOpIO >=> interpretTapeOp

interpretBF :: (Monad m) => Producer Word8 m () -> Consumer Word8 m () -> ILProgram Int -> m ()
interpretBF inp outp program = runEffect $ inp >-> evalStateP (tapeOf 0) (interpret program) >-> outp
