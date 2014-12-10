{-# LANGUAGE BangPatterns #-}


module Brainhask.Interpreter (interpretBF) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.ByteString.Internal
import Data.Tape
import Data.Word (Word8)
import Brainhask.Optimizer
import Brainhask.Types

type Memory = Tape Word8

initialMemory :: Memory
initialMemory = tapeOf 0

interpretOp :: Op Int -> StateT Memory IO ()
interpretOp NoOp = return ()

interpretOp !(Move   0) = return ()
interpretOp !(Move   n) = modify $! moveRight n
interpretOp !(Modify n) = modify $! modifyCursor $ \x ->  fromIntegral n  + x  :: Word8
interpretOp !(Set    n) = modify $! replaceCursor $ fromIntegral n
interpretOp !(Put 0) = return ()
interpretOp !(Put n) = do
    c <- _cursor <$> get
    liftIO $ putStr $ replicate n (w2c c)

interpretOp !(Get 0) = return ()
interpretOp !(Get 1) = do
    c <- liftIO $ putStr "\n> " >> getChar
    liftIO $ putStrLn ""
    modify $! replaceCursor $! c2w c

interpretOp !(Get _) = interpretOp (Get 1)

interpretOp !(Loop []) = return ()
interpretOp !(Loop ops) = do
    m <- get
    when (_cursor m /= 0) $! interpret (ops ++ [Loop ops])

interpret :: Program Int -> StateT Memory IO ()
interpret = mapM_ interpretOp

interpretBF :: Program Int -> IO ()
interpretBF = void . flip execStateT initialMemory . interpret
