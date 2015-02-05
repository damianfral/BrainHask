module Language.BrainHask.Parser (parseBF) where

import           Control.Applicative      hiding (optional)
import           Language.BrainHask.Types
import           Text.Parsec              hiding (many)

type Parser  = Parsec String ()

howMany1 :: String -> Parser Int
howMany1 xs = length <$> many1 (string  xs)

programParser :: Parser (BFProgram Int)
programParser = many opParser

opParser, rightParser, leftParser, incParser, decParser, putParser, getParser, loopParser :: Parser (Op Int)

opParser    = choice [ rightParser, leftParser, incParser, decParser, putParser, getParser, loopParser ]
rightParser = Move            <$> howMany1 ">"
leftParser  = Move   . negate <$> howMany1 "<"
incParser   = Add             <$> howMany1 "+"
decParser   = Add    . negate <$> howMany1 "-"
putParser   = Put             <$> howMany1 "."
getParser   = Get             <$> howMany1 ","
loopParser  = between (string "[") (string "]") $ Loop <$> programParser

clean :: String -> String
clean = filter (`elem` "+-<>[].,")

parseBF :: String -> Either ParseError (BFProgram Int)
parseBF = runParser programParser () "" . clean
