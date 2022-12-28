{-# LANGUAGE OverloadedStrings #-}

module Language.BrainHask.Interpreter
  ( MachineMemory,
    MachineIO (..),
    Machine (..),
    MachineM,
    interpretBF,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import Data.Tape
import Data.Word (Word8)
import Language.BrainHask.Types

type MachineMemory = Tape Word8

data MachineIO = MachineIO {_read :: IO Word8, _write :: [Word8] -> IO ()}

data Machine = Machine MachineMemory MachineIO

type MachineM = RWST MachineIO () MachineMemory IO

interpretOpIO :: Op Int -> MachineM (Op Int)
interpretOpIO (Get n)
  | n <= 0 = return NoOp
  | otherwise = do
      (MachineIO read' _) <- ask
      c <- liftIO read'
      return $ Set $ fromIntegral c
interpretOpIO (Put n)
  | n <= 0 = return NoOp
  | otherwise = do
      (MachineIO _ write') <- ask
      c <- _cursor <$> get
      liftIO $ write' $ replicate n c
      return NoOp
interpretOpIO c = return c

interpretTapeOp :: Op Int -> MachineM ()
interpretTapeOp NoOp = return ()
interpretTapeOp (Move n) = modify $! moveRight n
interpretTapeOp (Add n) = modify $! modifyCursor $ (+) (fromIntegral n)
interpretTapeOp (Set n) = modify $! replaceCursor $ fromIntegral n
interpretTapeOp (Loop []) = return ()
interpretTapeOp (Loop ops) = do
  c <- _cursor <$> get
  when (c /= 0) $! interpret (ops ++ [Loop ops])
interpretTapeOp _ = return ()

interpret :: BFProgram Int -> MachineM ()
interpret = mapM_ $ interpretOpIO >=> interpretTapeOp

interpretBF :: Machine -> BFProgram Int -> IO ()
interpretBF (Machine mMemory mIO) = void . (\x -> runRWST x mIO mMemory) . interpret
