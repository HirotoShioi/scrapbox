{-| Test suites for 'runScrapTextParser'
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.ScrapText
    ( scrapTextParserSpec
    ) where

import           RIO hiding (span)

import           RIO.List (headMaybe)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Property, choose, listOf1,
                                  scale)

import           Data.Scrapbox (InlineBlock (..), ScrapText (..), Segment (..),
                                Style (..), Url (..))
import           Data.Scrapbox.Internal (concatSegment, isBold, isCodeNotation,
                                         isItalic, isMathExpr, isStrikeThrough,
                                         renderSegments, runScrapTextParser)
import           TestScrapboxParser.Utils (ScrapboxSyntax (..), checkContent,
                                           checkParsed, propParseAsExpected)
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

        describe "Inline blocks" $ modifyMaxSuccess (const 100) $ do
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

instance ScrapboxSyntax MathExpr where
    render (MathExpr txt)     = "[$ " <> txt <> "]"
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
    arbitrary = CodeNotation <$> genText

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

-- SPAN
-- bold
-- heading
-- italic
-- strikethrough

data SpanStyle
    = PlainStyle
    | BoldStyle
    | ItalicStyle
    | StrikeThroughStyle

newtype StyledSpan (a :: SpanStyle) = StyledSpan
    { getStyledSpan :: [Segment]
    } deriving Show

instance Arbitrary (StyledSpan a) where
    arbitrary = do
        newSize <- choose (0, sizeNum)
        scale (\size -> if size < sizeNum then size else newSize) $
            StyledSpan . concatSegment . addSpace <$> listOf1 arbitrary
        where
          -- Add space after hashtag
          addSpace :: [Segment] -> [Segment]
          addSpace []                 = []
          addSpace [HASHTAG txt]      = [HASHTAG txt]
          addSpace (HASHTAG txt:rest) = HASHTAG txt : TEXT " " : addSpace rest
          addSpace (x:xs)             = x : addSpace xs

          sizeNum :: Int
          sizeNum = 10

instance ScrapboxSyntax (StyledSpan 'PlainStyle) where
    render (StyledSpan segments)     = renderSegments segments
    getContent (StyledSpan segments) = renderSegments segments

instance ScrapboxSyntax (StyledSpan 'BoldStyle) where
    render (StyledSpan segments)     = "[* " <> renderSegments segments <> "]"
    getContent (StyledSpan segments) = renderSegments segments

instance ScrapboxSyntax (StyledSpan 'ItalicStyle) where
    render (StyledSpan segments)     = "[/ " <> renderSegments segments <> "]"
    getContent (StyledSpan segments) = renderSegments segments

instance ScrapboxSyntax (StyledSpan 'StrikeThroughStyle) where
    render (StyledSpan segments)     = "[- " <> renderSegments segments <> "]"
    getContent (StyledSpan segments) = renderSegments segments

styledSpanSpec :: Spec
styledSpanSpec = describe "Styled inlines" $ do
    describe "Non-Styled" $ do
        prop "should parse as Non-styled" $
            \(plainInline :: StyledSpan 'PlainStyle) ->
                testParse plainInline null
        prop "should preserve its content" $
            \(plainInline :: StyledSpan 'PlainStyle) ->
                testContent plainInline

    describe "Bold" $ do
        prop "should parse as Bold" $
            \(boldInline :: StyledSpan 'BoldStyle) ->
                testParse
                    boldInline
                    (\styles -> length styles == 1 && all isBold styles)
        prop "should preserve its content" $
            \(boldInline :: StyledSpan 'BoldStyle) ->
                testContent boldInline

    describe "Italic" $ do
        prop "should parse as Bold" $
            \(italicInline :: StyledSpan 'ItalicStyle) ->
                testParse
                    italicInline
                    (\styles -> length styles == 1 && all isItalic styles)
        prop "should preserve its content" $
            \(italicInline :: StyledSpan 'ItalicStyle) ->
                testContent italicInline

    describe "StrikeThrough" $ do
        prop "should parse as StrikeThrough" $
            \(strikeThroughInline :: StyledSpan 'StrikeThroughStyle) ->
                testParse
                    strikeThroughInline
                    (\styles -> length styles == 1 && all isStrikeThrough styles)

        prop "should preserve its content" $
            \(strikeThroughInline :: StyledSpan 'StrikeThroughStyle) ->
                testContent strikeThroughInline
  where
    getSpan :: InlineBlock -> Maybe InlineBlock
    getSpan span@(SPAN _ _) = Just span
    getSpan _               = Nothing

    testParse :: (ScrapboxSyntax (StyledSpan a))
              => StyledSpan a
              -> ([Style] -> Bool)
              -> Property
    testParse inlineBlock = checkParsed inlineBlock runScrapTextParser
        (\(ScrapText inlines) -> do
            guard $ length inlines == 1
            inline <- headMaybe inlines
            (SPAN style _) <- getSpan inline
            return style
        )

    testContent :: (ScrapboxSyntax (StyledSpan a)) => StyledSpan a -> Property
    testContent inlineBlock = checkParsed inlineBlock runScrapTextParser
        (\(ScrapText inlines) -> do
            guard $ length inlines == 1
            inline <- headMaybe inlines
            (SPAN _ segments) <- getSpan inline
            return segments
        )
        (== getStyledSpan inlineBlock)
