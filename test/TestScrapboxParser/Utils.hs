{-| Utility functions used within TestScrapboxParser
-}

{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.Utils
    ( propParseAsExpected
    ) where

import           RIO

import           Test.QuickCheck (Property, Testable (..), (===))
import           Text.Parsec (ParseError)

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | General unit testing to see the parser can parse given data as expected
propParseAsExpected :: (Eq parsed, Show parsed)
                    => toParse
                    -> parsed
                    -> (toParse -> Either ParseError parsed)
                    -> Property
propParseAsExpected example expected parser = property $ either
    (const $ property False)
    (=== expected)
    (parser example)
