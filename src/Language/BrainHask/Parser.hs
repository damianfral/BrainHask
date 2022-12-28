module Language.BrainHask.Parser (parseBF) where

import Control.Applicative hiding (optional)
import Language.BrainHask.Types
import Text.Parsec hiding (many)

type Parser = Parsec String ()

toOpParser :: String -> BFOp -> Parser BFOp
toOpParser s p = p <$ string s

programParser :: Parser BFProgram
programParser = many opParser

opParser = choice [rightParser, leftParser, incParser, decParser, writeParser, readParser, loopParser]

rightParser = toOpParser ">" BFMoveRight

leftParser = toOpParser "<" BFMoveLeft

incParser = toOpParser "+" BFIncrease

decParser = toOpParser "-" BFDecrease

writeParser = toOpParser "." BFWrite

readParser = toOpParser "," BFRead

loopParser = between (string "[") (string "]") $ BFLoop <$> programParser

clean :: String -> String
clean = filter (`elem` "+-<>[].,")

parseBF :: String -> Either ParseError BFProgram
parseBF = runParser programParser () "" . clean
