module Brainhask.Parser (Op(..), Program(..), parseBF) where

import Control.Applicative
import Text.Parsec hiding ((<|>), many)

data Op      = MoveR | MoveL | Inc | Dec | Put | Get | Loop [Op] deriving (Show, Eq)
type Program = [Op]

type Parser  = Parsec String ()

programParser :: Parser Program
programParser = many opParser

opParser, rightParser, leftParser, incParser, decParser, putParser, getParser, loopParser :: Parser Op
opParser    = choice [ rightParser, leftParser, incParser, decParser, putParser, getParser, loopParser ]
rightParser = string ">" *> pure MoveR
leftParser  = string "<" *> pure MoveL
incParser   = string "+" *> pure Inc
decParser   = string "-" *> pure Dec
putParser   = string "." *> pure Put
getParser   = string "," *> pure Get
loopParser  = between (string "[") (string "]") $ Loop <$> many opParser

clean :: String -> String
clean = filter (`elem` "+-<>[].,")

parseBF :: String -> Either ParseError Program
parseBF = runParser programParser () "" . clean
