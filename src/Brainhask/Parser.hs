module Brainhask.Parser (parseBF) where

import Brainhask.Types
import Control.Applicative hiding (optional)
import Text.Parsec hiding (many)

type Parser  = Parsec String ()

programParser :: Parser (Program Int)
programParser = many opParser

opParser, rightParser, leftParser, incParser, decParser, putParser, getParser, loopParser, noopParser :: Parser (Op Int)
opParser    = choice [ rightParser, leftParser, incParser, decParser, putParser, getParser, loopParser ]
rightParser = Move   . length          <$> many1 (string ">")
leftParser  = Move   . negate . length <$> many1 (string "<")
incParser   = Modify . length          <$> many1 (string "+" )
decParser   = Modify . negate . length <$> many1 (string "-" )
putParser   = Put    . length          <$> many1 (string "." )
getParser   = Get    . length          <$> many1 (string "," )
loopParser  = between (string "[") (string "]") $ Loop <$> programParser
noopParser  = many (noneOf "<>,.+-[]") *> pure NoOp

clean :: String -> String
clean = filter (`elem` "+-<>[].,")

parseBF :: String -> Either ParseError (Program Int)
parseBF = runParser programParser () "" . clean
