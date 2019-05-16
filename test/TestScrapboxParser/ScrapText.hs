{-| Test suites for 'runScrapTextParser'
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.ScrapText
    ( scrapTextParserSpec
    ) where

import           RIO

import           RIO.List (headMaybe)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), property, (===))

import           Data.Scrapbox (InlineBlock (..), ScrapText (..), Segment (..),
                                Style (..), Url (..))
import           Data.Scrapbox.Internal (concatSegment, isCodeNotation,
                                         isMathExpr,
                                         renderWithStyle, runScrapTextParser,
                                         shortListOf)
import           TestScrapboxParser.Utils (Syntax (..), checkParsed,
                                           propParseAsExpected)
import           Utils (genText, propNonNull, shouldParseSpec)

-- | Test spec for scrap text parser
scrapTextParserSpec :: Spec
scrapTextParserSpec =
    describe "ScrapText parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runScrapTextParser

        prop "should return non-empty list of contexts if the given string is non-empty" $
            propNonNull runScrapTextParser (\(ScrapText inlines) -> inlines)

        it "should parse given example text as expected" $
             propParseAsExpected exampleText expectedParsedText runScrapTextParser

        describe "Inline blocks" $ do
            mathExprSpec
            codeNotationSpec
            styledSpanSpec

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

-- | Math expression syntax
newtype MathExpr = MathExpr Text
    deriving Show

instance Arbitrary MathExpr where
    arbitrary = MathExpr <$> genText

instance Syntax MathExpr where
    render (MathExpr txt)     = "[$ " <> txt <> "]"

-- math expressions
mathExprSpec :: Spec
mathExprSpec = describe "Math Expressions" $ do
    prop "Should parse math expression syntax as MATH_EXPRESSION" $
        \(mathExpr :: MathExpr) ->
            checkParsed
                mathExpr
                runScrapTextParser
                (\(ScrapText inlines) -> headMaybe inlines)
                (property . isMathExpr)

    prop "Should preserve its content" $
        \(mathExpr@(MathExpr expr)) ->
            checkParsed
                mathExpr
                runScrapTextParser
                (\(ScrapText inlines) -> headMaybe inlines)
                (=== MATH_EXPRESSION expr)

-- code notation
newtype CodeNotation = CodeNotation Text
    deriving Show

instance Arbitrary CodeNotation where
    arbitrary = CodeNotation <$> genText

instance Syntax CodeNotation where
    render (CodeNotation text)     = "`" <> text <> "`"

codeNotationSpec :: Spec
codeNotationSpec = describe "Code notation" $ do
    prop "should parse code notation as it should be" $
        \(codeNotation :: CodeNotation) ->
            checkParsed codeNotation runScrapTextParser
                (\(ScrapText inlines) -> headMaybe inlines)
                (property . isCodeNotation)

    prop "should preserve its content" $
        \(codeNotation@(CodeNotation notation)) ->
            checkParsed codeNotation runScrapTextParser
                (\(ScrapText inlines) -> do
                    guard (length inlines == 1)
                    headMaybe inlines
                )
                (=== CODE_NOTATION notation)

-- SPAN
-- bold
-- heading
-- italic
-- strikethrough

data StyledSpan = StyledSpan
    { spanStyle   :: ![Style]
    , spanContent :: ![Segment]
    } deriving Show

instance Arbitrary StyledSpan where
    arbitrary = do
         randomStyle    <- arbitrary
         randomSegments <- concatSegment . addSpace <$> shortListOf arbitrary
         return $ StyledSpan randomStyle randomSegments
        where
          -- Add space after hashtag
          addSpace :: [Segment] -> [Segment]
          addSpace []                 = []
          addSpace [HASHTAG txt]      = [HASHTAG txt]
          addSpace (HASHTAG txt:rest) = HASHTAG txt : TEXT " " : addSpace rest
          addSpace (x:xs)             = x : addSpace xs

instance Syntax StyledSpan where
    render (StyledSpan styles segments) = renderWithStyle styles segments

styledSpanSpec :: Spec
styledSpanSpec = describe "Styled inlines" $
    prop "should parse model" $
        \(styledSpan@(StyledSpan styles segments)) ->
            checkParsed
                styledSpan
                runScrapTextParser
                (\(ScrapText inlines) -> do
                    guard $ length inlines == 1
                    headMaybe inlines
                )
                (=== SPAN styles segments)
