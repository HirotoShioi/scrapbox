{-| Utility function used within parser modules
-}

module Data.Scrapbox.Parser.Scrapbox.Utils
    ( lookAheadMaybe
    ) where

import           RIO                           hiding (try)

import           Text.ParserCombinators.Parsec (Parser, lookAhead, optionMaybe,
                                                try)

-- | Try to parse ahead content with given 'Parser a' and return its result with 'Maybe a'
lookAheadMaybe :: Parser a -> Parser (Maybe a)
lookAheadMaybe parser = lookAhead . optionMaybe $ try parser
