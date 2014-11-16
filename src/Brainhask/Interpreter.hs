module Brainhask.Interpreter (interpretBF) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.ByteString.Internal
import Data.Tape
import Data.Word (Word8)
import Brainhask.Parser

type Memory = Tape Word8

initialMemory :: Memory
initialMemory = tapeOf 0

interpretOp :: Op -> StateT Memory IO ()
interpretOp MoveR = modify moveRight
interpretOp MoveL = modify moveLeft
interpretOp Inc   = modify $ modifyCursor ( 1 +)
interpretOp Dec   = modify $ modifyCursor (-1 +)

interpretOp Put   = do
    c <- _cursor <$> get
    liftIO $ putChar (w2c c)

interpretOp Get   = do
    liftIO $
    c <- liftIO $ putStr "\n> " >> getChar
    liftIO $ putStrLn ""
    modify $ replaceCursor $ c2w c

interpretOp (Loop ops) = do
    m <- get
    when (_cursor m /= 0) $ interpret (ops ++ [Loop ops])

interpret :: Program -> StateT Memory IO ()
interpret = mapM_ interpretOp

interpretBF :: Program -> IO ()
interpretBF = void . flip execStateT initialMemory . interpret
