{-| Utility functions used within TestScrapboxParser
-}

{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.Utils
    ( ScrapboxSyntax(..)
    , propParseAsExpected
    , checkContent
    , checkParsed
    ) where

import           RIO

import qualified RIO.Text as T
import           Test.QuickCheck (Property, Testable (..))
import           Text.Parsec (ParseError)

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

-- | Type class used to render/get content of given syntax
class ScrapboxSyntax a where
    render     :: a -> Text
    getContent :: a -> Text

-- | Check parsed
checkParsed :: (ScrapboxSyntax syntax)
            => syntax
            -- ^ Syntax that we want to test on
            -> (String -> Either ParseError a)
            -- ^ Parser
            -> (a -> Maybe b)
            -- ^ Getter
            -> (b -> Bool)
            -- ^ Predicate
            -> Property
checkParsed syntax parser getter pre = property $ either
    (const False)
    (maybe False pre . getter)
    (parser $ T.unpack $ render syntax)

-- | Test case to check whether the parsed thing still preserves its content
checkContent :: (ScrapboxSyntax syntax)
             => syntax
             -- ^ Syntax that we want to test on
             -> (String -> Either ParseError a)
             -- ^ Parser
             -> (a -> Maybe Text)
             -- ^ Getter
             -> Property
checkContent syntax parser getter =
    checkParsed syntax parser getter (\txt -> txt == getContent syntax)
