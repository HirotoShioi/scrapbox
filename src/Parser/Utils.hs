module Parser.Utils where

import           RIO                           hiding (try)

import           Text.ParserCombinators.Parsec

lookAheadMaybe :: Parser a -> Parser (Maybe a)
lookAheadMaybe parser = lookAhead . optionMaybe $ try parser
