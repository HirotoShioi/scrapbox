{-| Test suites for 'runScrapTextParser'
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.ScrapText
    ( scrapTextParserSpec
    ) where

import           RIO                      hiding (assert)

import           RIO.List                 (headMaybe)
import           Test.Hspec               (Spec, describe, it)
import           Test.Hspec.QuickCheck    (modifyMaxSuccess, prop)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic  (assert, monadicIO)

import           Parser.ScrapText         (runScrapTextParser)
import           TestScrapboxParser.Utils (NonEmptyPrintableString (..),
                                           ScrapboxSyntax (..), checkContent,
                                           checkParsed, genPrintableText,
                                           propParseAsExpected, shouldParseSpec)
import           Types                    (InlineBlock (..), ScrapText (..),
                                           Segment (..), Style (..), Url (..),
                                           isMathExpr)
import           Utils                    (whenRight)

-- | Test spec for scrap text parser
scrapTextParserSpec :: Spec
scrapTextParserSpec =
    describe "ScrapText parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runScrapTextParser

        prop "should return non-empty list of contexts if the given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runScrapTextParser $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \(ScrapText inlines) ->
                    assert $ not $ null inlines

        it "should parse given example text as expected" $
             propParseAsExpected exampleText expectedParsedText runScrapTextParser

        describe "Math Expressions" $ modifyMaxSuccess (const 200) $ do
            prop "Should parse math expression syntax as MATH_EXPRESSION" $
                \(mathExpr :: MathExpr) ->
                    checkParsed
                        mathExpr
                        runScrapTextParser
                        (\(ScrapText inlines) -> headMaybe inlines)
                        isMathExpr

            prop "Should preserve its content" $
                \(mathExpr :: MathExpr) ->
                    checkContent
                        mathExpr
                        runScrapTextParser
                        (\(ScrapText inlines) -> do
                            inline  <- headMaybe inlines
                            getMathExprContent inline
                        )
  where
    getMathExprContent :: InlineBlock -> Maybe Text
    getMathExprContent (MATH_EXPRESSION expr) = Just expr
    getMathExprContent _                      = Nothing

    exampleText :: String
    exampleText = "[* bold text] [- strikethrough text] [/ italic text] simple text `code_notation` [* test [link] test [partial]"

    expectedParsedText :: ScrapText
    expectedParsedText = ScrapText
        [ ITEM Bold [ TEXT "bold text" ]
        , ITEM NoStyle [ TEXT " " ]
        , ITEM StrikeThrough [ TEXT "strikethrough text" ]
        , ITEM NoStyle [ TEXT " " ]
        , ITEM Italic [ TEXT "italic text" ]
        , ITEM NoStyle [ TEXT " simple text " ]
        , CODE_NOTATION "code_notation"
        , ITEM NoStyle [ TEXT " " ]
        , ITEM Bold
            [ TEXT "test "
            , LINK Nothing ( Url "link" )
            , TEXT " test [partial"
            ]
        ]

-- | Math expression syntax
newtype MathExpr = MathExpr Text
    deriving Show

instance Arbitrary MathExpr where
    arbitrary = MathExpr <$> genPrintableText

instance ScrapboxSyntax MathExpr where
    render (MathExpr txt)     = "[$" <> txt <> "]"
    getContent (MathExpr txt) = txt
