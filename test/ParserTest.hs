{- Test suites for scrapbox parser
-}

{-# LANGUAGE ScopedTypeVariables #-}

module ParserTest where

import           RIO                     hiding (assert)

import           Test.Hspec              (Spec, describe)
import           Test.Hspec.QuickCheck   (modifyMaxSuccess, prop)
import           Test.QuickCheck         (Arbitrary (..), PrintableString (..),
                                          arbitraryPrintableChar, listOf1)
import           Test.QuickCheck.Monadic (assert, monadicIO)
import           Text.Parsec             (ParseError)

import           Parser.Inline           (runInlineParser)
import           Parser.Text             (runScrapTextParser)
import           Types
import           Utils                   (whenRight)

-- | Test spec for inline parser
parserSpec :: Spec
parserSpec = do
    inlineParserSpec
    scrapTextParserSpec

inlineParserSpec :: Spec
inlineParserSpec = 
    describe "inline parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runInlineParser

        prop "should return non-empty list of segments if given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runInlineParser $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \parsedContent ->
                    assert $ not $ null parsedContent

scrapTextParserSpec :: Spec
scrapTextParserSpec =
    describe "scrap text parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runScrapTextParser

        prop "should return non-empty list of contexts if the given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runScrapTextParser $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \(ScrapText ctxs) ->
                    assert $ not $ null ctxs


newtype NonEmptyPrintableString =  NonEmptyPrintableString {
    getNonEmptyPrintableString :: String
    } deriving Show

instance Arbitrary NonEmptyPrintableString where
    arbitrary = NonEmptyPrintableString <$> listOf1 arbitraryPrintableChar

shouldParseSpec :: (String -> Either ParseError a) -> Spec
shouldParseSpec parser =
        prop "should be able to parse any text without failing or cause infinite loop" $
            \(someText :: PrintableString) ->
                isRight $ parser $ getPrintableString someText
