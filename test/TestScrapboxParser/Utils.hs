{-| Utility functions used within TestScrapboxParser
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.Utils
    ( NonEmptyPrintableString(..)
    , ScrapboxSyntax(..)
    , propParseAsExpected
    , shouldParseSpec
    , checkContent
    , checkParsed
    ) where

import           RIO                     hiding (assert)

import qualified RIO.Text                as T
import           Test.Hspec              (Spec)
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck         (Arbitrary (..), PrintableString (..),
                                          Property, Testable (..),
                                          arbitraryPrintableChar, listOf1)
import           Test.QuickCheck.Monadic (assert, monadicIO)
import           Text.Parsec             (ParseError)

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Non-empty version of 'PrintableString'
newtype NonEmptyPrintableString =  NonEmptyPrintableString
    { getNonEmptyPrintableString :: String
    } deriving Show

instance Arbitrary NonEmptyPrintableString where
    arbitrary = NonEmptyPrintableString <$> listOf1 arbitraryPrintableChar

-- | General testing spec for parser
shouldParseSpec :: (String -> Either ParseError a) -> Spec
shouldParseSpec parser =
        prop "should be able to parse any text without failing or cause infinite loop" $
            \(someText :: PrintableString) ->
                isRight $ parser $ getPrintableString someText

-- | General unit testing to see the parser can parse given data as expected
propParseAsExpected :: (Eq parsed)
                    => toParse
                    -> parsed
                    -> (toParse -> Either ParseError parsed)
                    -> Property
propParseAsExpected example expected parser = monadicIO $ either
    (\parseError    -> fail $ "Failed to parse with error: " <> show parseError)
    (\parsedContent -> assert $ parsedContent == expected)
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
