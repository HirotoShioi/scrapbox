{-| Utility functions used within TestScrapboxParser
-}

{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.Utils
    ( Syntax(..)
    , propParseAsExpected
    , checkParsed
    ) where

import           RIO

import qualified RIO.Text as T
import           Test.QuickCheck (Property, Testable (..))
import           Text.Parsec (ParseError)
import           Utils (Syntax (..))

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | General unit testing to see the parser can parse given data as expected
propParseAsExpected :: (Eq parsed)
                    => toParse
                    -> parsed
                    -> (toParse -> Either ParseError parsed)
                    -> Property
propParseAsExpected example expected parser = property $ either
    (const False)
    (== expected)
    (parser example)

-- | Check parsed
checkParsed :: (Syntax syntax)
            => syntax
            -- ^ Syntax that we want to test on
            -> (String -> Either ParseError a)
            -- ^ Parser
            -> (a -> Maybe b)
            -- ^ Getter
            -> (b -> Property)
            -- ^ Predicate
            -> Property
checkParsed syntax parser getter pre = either
    (const failed)
    (maybe failed pre . getter)
    (parser $ T.unpack $ render syntax)
  where
    failed = property False
