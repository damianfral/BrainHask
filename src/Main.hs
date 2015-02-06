{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Data.ByteString.Internal
import           Data.Functor
import           Data.Either.Combinators
import           Data.Tape
import           Language.BrainHask

#ifdef __GHCJS__

import GHCJS.Types
import GHCJS.Foreign

#else

import           Options.Applicative
import           System.Directory
import           System.IO

#endif

data Options = Options
    { _input    :: String
    , _optimize :: Bool
    , _ast      :: Bool }


#ifdef __GHCJS__

data JQuery_
type JQuery = JSRef JQuery_

foreign import javascript unsafe
    "jQuery($1)"
    jq_select :: JSString -> IO (JQuery)

foreign import javascript unsafe
    "$1.click($2)"
    jq_onClick :: JQuery -> JSFun (IO ()) -> IO ()

foreign import javascript unsafe
    "jQuery($1).val()"
    jq_getVal :: JQuery -> IO (JSString)

foreign import javascript unsafe
    "jQuery($1).val($2)"
    jq_setVal :: JQuery -> JSString -> IO ()

foreign import javascript unsafe
    "prompt()"
    js_prompt :: IO JSString

select :: String -> IO JQuery
select = jq_select . toJSString

onClick :: JQuery -> IO () -> IO ()
onClick jq io = do
    cb <- asyncCallback NeverRetain io
    jq_onClick jq cb

getVal :: JQuery -> IO String
getVal jq = fromJSString <$> jq_getVal jq

setVal :: JQuery -> String -> IO ()
setVal jq = jq_setVal jq . toJSString

prompt :: IO (String)
prompt = fromJSString <$> js_prompt

appendVal :: JQuery -> String -> IO ()
appendVal jq val = do
    current <- getVal jq
    setVal jq $ current ++ val

readInput :: FilePath -> IO (Either String String)
readInput inp = do
    input <- select inp
    Right <$> getVal input

initMachine :: IO Machine
initMachine = Machine machineMemory <$> machineIO
    where
        machineMemory = tapeOf 0
        machineIO     = do
            output <- select "#output"
            return $ MachineIO (c2w . head <$> prompt) (appendVal output . map w2c)

runClick = runOptions (Options "#input" True False)
astClick = runOptions (Options "#input" True True)

main :: IO ()
main = do
    select "#run"   >>= flip onClick runClick
    select "#ast"   >>= flip onClick astClick
    select "#clear" >>= flip onClick (select "#output" >>= flip setVal "")

#else

options :: Parser Options
options = Options
    <$> argument str (metavar "FILENAME")
    <*> switch (short 'o' <> long "optimize" <> help "Try to optimize")
    <*> switch (short 'a' <> long "ast"      <> help "Print the AST")


readInput :: FilePath -> IO (Either String String)
readInput fn = doesFileExist fn >>= \b ->
    if b then     Right <$> readFile fn
    else return $ Left $ fn ++ " does not exist"

initMachine :: IO Machine
initMachine = do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    return $ Machine machineMemory machineIO
    where
        machineMemory = tapeOf 0
        machineIO     = MachineIO (putStr "\n> " >> (c2w <$> getChar)) (putStr . map w2c)

main :: IO ()
main = execParser opts >>= runOptions
    where opts = info ( helper <*> options )
                      ( fullDesc <> progDesc "Brainfuck interpreter" )

#endif

runOptions :: Options -> IO ()
runOptions (Options input o a) = do
    machine <- initMachine
    string  <- readInput input
    let program = string >>= mapLeft show . parseBF
    go o a machine program
    where
        go _ _ (Machine _ (MachineIO _ wf)) ( Left errorMsg ) = wf $ map c2w errorMsg
        go o' a' machine@(Machine _ (MachineIO _ wf)) ( Right program )
            | o' && a'    = wf . map c2w . show $ optimize program
            | o'          = interpretBF machine $ optimize program
            | a'          = wf . map c2w . show $ program
            | otherwise   = interpretBF machine program
