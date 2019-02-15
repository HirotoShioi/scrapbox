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
                                           isCodeNotation, isMathExpr)
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

        describe "Indivisual data" $ modifyMaxSuccess (const 200) $ do
            mathExprSpec
            codeNotationSpec

  where
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

-- math expressions
mathExprSpec :: Spec
mathExprSpec = describe "Math Expressions" $ do
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

-- code notation
newtype CodeNotation = CodeNotation Text
    deriving Show

instance Arbitrary CodeNotation where
    arbitrary = CodeNotation <$> genPrintableText

instance ScrapboxSyntax CodeNotation where
    render (CodeNotation text)     = "`" <> text <> "`"
    getContent (CodeNotation text) = text

codeNotationSpec :: Spec
codeNotationSpec = describe "Code notation" $ do
    prop "should parse code notation as it should be" $
        \(codeNotation :: CodeNotation) ->
            checkParsed codeNotation runScrapTextParser
                (\(ScrapText inlines) -> headMaybe inlines)
                isCodeNotation
    
    prop "should preserve its content" $
        \(codeNotation :: CodeNotation) ->
            checkContent codeNotation runScrapTextParser
                (\(ScrapText inlines) -> do
                    guard (length inlines == 1)
                    inline <- headMaybe inlines
                    (CODE_NOTATION txt) <- getCodes inline
                    return txt
                )
  where
    getCodes :: InlineBlock -> Maybe InlineBlock
    getCodes c@(CODE_NOTATION _) = Just c
    getCodes _                   = Nothing

-- item
-- bold
-- italic
-- strikethrough
