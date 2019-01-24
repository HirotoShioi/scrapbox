{- Test suites for scrapbox parser
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParserTest where

import           RIO                     hiding (assert)

import           Test.Hspec              (Spec, describe, it)
import           Test.Hspec.QuickCheck   (modifyMaxSuccess, prop)
import           Test.QuickCheck         (Arbitrary (..), PrintableString (..),
                                          arbitraryPrintableChar, listOf1)
import           Test.QuickCheck.Monadic (assert, monadicIO)
import           Text.Parsec             (ParseError)

import           Parser.Inline           (runInlineParser)
import           Parser.Text             (runScrapTextParser)
import           Types
import           Utils                   (whenRight)

-- | Test spec for scrapbox parser
parserSpec :: Spec
parserSpec = do
    inlineParserSpec
    scrapTextParserSpec


-- | Spec for inline text parser
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

-- | Test spec for scrap text parser
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

        it "should parse given example text as expected" $ monadicIO $ do
            let eParseredText = runScrapTextParser exampleText

            assert $ isRight eParseredText

            whenRight eParseredText $ \parsedText ->
                assert $ parsedText == expectedParsedText
  where
    exampleText :: String
    exampleText = "[* bold text] [- strikethrough text] [/ italic text] simple text [* test [link] test [partial]"

    expectedParsedText :: ScrapText
    expectedParsedText = ScrapText
            [ Context Bold [ SimpleText "bold text" ]
            , Context NoStyle [ SimpleText " " ]
            , Context StrikeThrough [ SimpleText "strikethrough text" ]
            , Context NoStyle [ SimpleText " " ]
            , Context Italic [ SimpleText "italic text" ]
            , Context NoStyle [ SimpleText " simple text " ]
            , Context Bold
                [ SimpleText "test "
                , Link Nothing ( Url "link" )
                , SimpleText " test [partial"
                ]
            ]

newtype NonEmptyPrintableString =  NonEmptyPrintableString {
    getNonEmptyPrintableString :: String
    } deriving Show

instance Arbitrary NonEmptyPrintableString where
    arbitrary = NonEmptyPrintableString <$> listOf1 arbitraryPrintableChar

-- | General testing spec for parser
shouldParseSpec :: (String -> Either ParseError a) -> Spec
shouldParseSpec parser =
        prop "should be able to parse any text without failing or cause infinite loop" $
            \(someText :: PrintableString) ->
                isRight $ parser $ getPrintableString someText
