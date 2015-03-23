{-# LANGUAGE CPP                #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Helper where

import           Data.ByteString.Internal
import           Data.Functor
import           Data.Word (Word8)
import           Data.Either.Combinators
import           Language.BrainHask
import           Pipes

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
    , _optimize :: OptimizationLevel
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

machineIO ::  (Producer Word8 IO (), Consumer Word8 IO ())
machineIO = (inp, outp)
    where
        inp    = do
            x <- (c2w . head <$> lift prompt)
            yield x
            inp
        outp  = do
            x      <- await
            output <- lift $ select "#output"
            lift $ appendVal output [w2c x]
            outp

runClick = runOptions (Options "#input" O2 False) machineIO
astClick = runOptions (Options "#input" O2 True ) machineIO

#else

options :: Parser Options
options = Options
    <$> argument str (metavar "FILENAME")
    <*> optimizationParser
    <*> switch (short 'a' <> long "ast"      <> help "Print the AST")

optimizationParser :: Parser OptimizationLevel
optimizationParser =  option
    ( str >>= \case
        "1" -> return O1
        "2" -> return O2
        _   -> return O0 )
    ( short 'o' <> long "optimization" <> help "Set optimization level [0,1,2]" )

readInput :: FilePath -> IO (Either String String)
readInput fn = doesFileExist fn >>= \b ->
    if b then     Right <$> readFile fn
    else return $ Left $ fn ++ " does not exist"

machineIO ::  (Producer Word8 IO (), Consumer Word8 IO ())
machineIO = (inp, outp)
    where
        inp    = do
            lift $ putStr "> "
            x <-  c2w <$> lift getChar
            yield x
            inp
        outp  = do
            x <- await
            lift $ putStr $ replicate 1 $ w2c x
            lift $ hFlush stdout
            outp
#endif

runOptions :: Options -> (Producer Word8 IO (), Consumer Word8 IO ()) -> IO ()
runOptions (Options input o a) (inp, outp)= do
    string  <- readInput input
    let program = string >>= mapLeft show . parseBF
    go program
    where
        go :: Either String BFProgram -> IO ()
        go ( Left errorMsg ) = runEffect $ each (map c2w errorMsg) >-> outp
        go ( Right program ) | a         = runEffect $ each (map c2w . show $ compile program) >-> outp
                             | otherwise = void $ interpretBF inp outp $ compile program
            where
                compile = optimize o . preprocess
