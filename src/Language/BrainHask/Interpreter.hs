{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveFunctor, DeriveDataTypeable #-}

module Language.BrainHask.Interpreter (interpretBF) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import Data.ByteString.Internal
import Data.Tape
import Data.Word (Word8)
import Language.BrainHask.Types
import Pipes

type Memory    = Tape Word8
data MachineIO = MachineIO { _read :: IO Int, _write :: Int -> IO () }
type Machine   = RWST MachineIO () Memory IO

initialMemory :: Memory
initialMemory = tapeOf 0

interpretOpIO :: Op Int -> Producer (Op Int) Machine ()
interpretOpIO (Get n) = when (n > 0) $ do
    (MachineIO read write) <- lift ask
    c <- liftIO $ putStr "\n> " >> getChar
    yield . Set . fromIntegral . c2w $ c

interpretOpIO (Put n) = when (n > 0) $ do
    c <- lift $ _cursor <$> get
    liftIO $ putStr $ replicate n $ w2c c

interpretOpIO c = yield c

interpretMemoryOp :: Op Int -> Effect Machine ()
interpretMemoryOp NoOp       = return ()
interpretMemoryOp (Move   n) = lift . modify $! moveRight n
interpretMemoryOp (Add    n) = lift . modify $! modifyCursor  $ (+) (fromIntegral n)
interpretMemoryOp (Set    n) = lift . modify $! replaceCursor $ fromIntegral n
interpretMemoryOp (Loop  []) = return ()
interpretMemoryOp (Loop ops) = do
    c <- _cursor <$> lift get
    when (c /= 0) $! interpret (ops ++ [Loop ops])
interpretMemoryOp _          = return ()

interpret :: BFProgram Int -> Effect Machine ()
interpret program = for (mapM_ interpretOpIO program) interpretMemoryOp

interpretBF :: BFProgram Int -> IO ()
interpretBF = void . (\x -> runRWST x machineIO initialMemory) . runEffect . interpret
    where
        machineIO = MachineIO (fromIntegral . c2w <$> getChar) (putStr . show)
