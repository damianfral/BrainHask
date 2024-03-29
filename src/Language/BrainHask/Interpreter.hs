{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.BrainHask.Interpreter
  ( MachineMemory,
    MachineM,
    interpretBF,
    machineIO,
  )
where

import Control.Monad (replicateM_, when, (>=>))
import Control.Monad.Trans.State.Strict
import Data.ByteString.Internal
import Data.Tape
import Data.Word (Word8)
import Language.BrainHask.Types
import Pipes
import Pipes.Lift
import System.IO (hFlush, stdout)

type MachineMemory = Tape Word8

type MachineM r = forall m. (Monad m) => Pipe Word8 Word8 (StateT MachineMemory m) r

interpretOpIO :: ILOp Int Int -> MachineM (ILOp Int Int)
interpretOpIO ILRead = ILSet . fromIntegral <$> await
interpretOpIO (ILWrite n)
  | n <= 0 = pure ILNoOp
  | otherwise = do
      c <- getCursor <$> lift get
      replicateM_ n $ yield c
      pure ILNoOp
interpretOpIO c = pure c

interpretTapeOp :: ILOp Int Int -> MachineM ()
interpretTapeOp ILNoOp = pure ()
interpretTapeOp (ILMove n) = lift $ modify $ moveCursor n
interpretTapeOp (ILAdd n) = lift $ modify $ updateCursor $ (+) (fromIntegral n)
interpretTapeOp (ILSet n) = lift $ modify $ replaceCursor $ fromIntegral n
interpretTapeOp (ILAddMult i n) = lift $ modify $ \v ->
  updateCursor (\x -> fromIntegral x + getIndex i v * fromIntegral n) v
interpretTapeOp (ILBlock ops) = interpret ops
interpretTapeOp (ILLoop []) = pure ()
interpretTapeOp (ILLoop ops) = do
  c <- getCursor <$> lift get
  when (c /= 0) $ interpret (ops ++ [ILLoop ops])
interpretTapeOp (ILAddTo n) = do
  c <- getCursor <$> lift get
  mapM_ interpretTapeOp [ILMove n, ILAdd (fromIntegral c), ILMove (-n)]
interpretTapeOp _ = pure ()

interpret :: ILProgram Int -> MachineM ()
interpret = mapM_ $ interpretOpIO >=> interpretTapeOp

interpretBF ::
  (Monad m) =>
  Producer Word8 m () ->
  Consumer Word8 m () ->
  ILProgram Int ->
  m ()
interpretBF inp outp program =
  runEffect $
    inp >-> evalStateP (tapeOf 0) (interpret program) >-> outp

machineIO :: (Producer Word8 IO (), Consumer Word8 IO ())
machineIO = (inp, outp)
  where
    inp = do
      lift $ putStr "> "
      x <- c2w <$> lift getChar
      yield x
      inp
    outp = do
      x <- await
      lift $ putStr [w2c x]
      lift $ hFlush stdout
      outp
