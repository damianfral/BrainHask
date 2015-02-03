{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveFunctor, DeriveDataTypeable #-}

module Language.BrainHask.Interpreter ( MachineMemory, MachineIO(..), Machine(..), MachineM, interpretBF) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import Data.Tape
import Data.Word (Word8)
import Language.BrainHask.Types
import Pipes

type MachineMemory = Tape Word8
data MachineIO     = MachineIO { _read :: IO Word8, _write :: [Word8] -> IO () }
data Machine       = Machine MachineMemory MachineIO

type MachineM      = RWST MachineIO () MachineMemory IO

interpretOpIO :: Op Int -> Producer (Op Int) MachineM ()
interpretOpIO (Get n) = when (n > 0) $ do
    (MachineIO read' _) <- lift ask
    c <- liftIO read'
    yield $ Set $ fromIntegral c

interpretOpIO (Put n) = when (n > 0) $ do
    (MachineIO _ write') <- lift ask
    c <- lift $ _cursor <$> get
    liftIO $ write' $ replicate n c

interpretOpIO c = yield c

interpretTapeOp :: Op Int -> Effect MachineM ()
interpretTapeOp NoOp       = return ()
interpretTapeOp (Move   n) = lift . modify $! moveRight n
interpretTapeOp (Add    n) = lift . modify $! modifyCursor  $ (+) (fromIntegral n)
interpretTapeOp (Set    n) = lift . modify $! replaceCursor $ fromIntegral n
interpretTapeOp (Loop  []) = return ()
interpretTapeOp (Loop ops) = do
    c <- _cursor <$> lift get
    when (c /= 0) $! interpret (ops ++ [Loop ops])
interpretTapeOp _          = return ()

interpret :: BFProgram Int -> Effect MachineM ()
interpret program = for (mapM_ interpretOpIO program) interpretTapeOp

interpretBF :: Machine -> BFProgram Int -> IO ()
interpretBF (Machine mMemory mIO) = void . (\x -> runRWST x mIO mMemory) . runEffect . interpret
