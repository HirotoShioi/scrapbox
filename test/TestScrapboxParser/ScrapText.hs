{-| Test suites for 'runScrapTextParser'
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.ScrapText
    ( scrapTextParserSpec
    , scrapTextRoundTripTest
    ) where

import           RIO

import qualified RIO.Text as T
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, property, (===))

import           Data.Scrapbox (InlineBlock (..), ScrapText (..), Segment (..),
                                Style (..), Url (..))
import           Data.Scrapbox.Internal (renderScrapText, runScrapTextParser)
import           TestScrapboxParser.Utils (propParseAsExpected)
import           Utils (propNonNull, shouldParseSpec)

-- | Test spec for scrap text parser
scrapTextParserSpec :: Spec
scrapTextParserSpec =
    describe "ScrapText parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runScrapTextParser

        prop "should return non-empty list of contexts if the given string is non-empty" $
            propNonNull runScrapTextParser (\(ScrapText inlines) -> inlines)

        it "should parse given example text as expected" $
             propParseAsExpected exampleText expectedParsedText runScrapTextParser

        describe "Inline blocks" $
            prop "round-trip test" scrapTextRoundTripTest

  where
    exampleText :: String
    exampleText = "[* bold text] [- strikethrough text] [/ italic text] simple text `code_notation` [* test [link] test [partial]"

    expectedParsedText :: ScrapText
    expectedParsedText = ScrapText
        [ SPAN [ Bold ] [ TEXT "bold text" ]
        , SPAN [] [ TEXT " " ]
        , SPAN [ StrikeThrough ] [ TEXT "strikethrough text" ]
        , SPAN [] [ TEXT " " ]
        , SPAN [ Italic ] [ TEXT "italic text" ]
        , SPAN [] [ TEXT " simple text " ]
        , CODE_NOTATION "code_notation"
        , SPAN [] [ TEXT " " ]
        , SPAN [ Bold ]
            [ TEXT "test "
            , LINK Nothing ( Url "link" )
            , TEXT " test [partial"
            ]
        ]

scrapTextRoundTripTest :: ScrapText -> Property
scrapTextRoundTripTest scrapText =
    let rendered = renderScrapText scrapText
    in either
        (const $ property  False)
        (=== scrapText)
        (runScrapTextParser $ T.unpack rendered)
