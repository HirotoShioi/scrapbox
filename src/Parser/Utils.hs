{-| Utility function used within parser modules
-}

module Parser.Utils
    ( lookAheadMaybe
    ) where

import           RIO                           hiding (try)

import           Text.ParserCombinators.Parsec

-- | Try to parse ahead content with given 'Parser a' and return its result with 'Maybe a'
lookAheadMaybe :: Parser a -> Parser (Maybe a)
lookAheadMaybe parser = lookAhead . optionMaybe $ try parser
