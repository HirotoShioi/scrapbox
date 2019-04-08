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

import           RIO hiding (assert)

import           RIO.List (headMaybe)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Property, choose, listOf1,
                                  scale)
import           Test.QuickCheck.Monadic (assert, monadicIO)

import           Data.Scrapbox (InlineBlock (..), ScrapText (..), Segment (..),
                                Style (..), Url (..))
import           Data.Scrapbox.Internal (concatSegment, isBold, isCodeNotation,
                                         isItalic, isMathExpr,
                                         isStrikeThrough, renderSegments,
                                         runScrapTextParser)
import           TestScrapboxParser.Utils (ScrapboxSyntax (..), checkContent,
                                           checkParsed, propParseAsExpected)
import           Utils (NonEmptyPrintableString (..), genPrintableText,
                        shouldParseSpec, whenRight)

-- | Test spec for scrap text parser
scrapTextParserSpec :: Spec
scrapTextParserSpec =
    describe "ScrapText parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runScrapTextParser

        prop "should return non-empty list of contexts if the given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runScrapTextParser
                        $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \(ScrapText inlines) ->
                    assert $ not $ null inlines

        it "should parse given example text as expected" $
             propParseAsExpected exampleText expectedParsedText runScrapTextParser

        describe "Inline blocks" $ modifyMaxSuccess (const 100) $ do
            mathExprSpec
            codeNotationSpec
            styledItemSpec

  where
    exampleText :: String
    exampleText = "[* bold text] [- strikethrough text] [/ italic text] simple text `code_notation` [* test [link] test [partial]"

    expectedParsedText :: ScrapText
    expectedParsedText = ScrapText
        [ ITEM [ Bold ] [ TEXT "bold text" ]
        , ITEM [] [ TEXT " " ]
        , ITEM [ StrikeThrough ] [ TEXT "strikethrough text" ]
        , ITEM [] [ TEXT " " ]
        , ITEM [ Italic ] [ TEXT "italic text" ]
        , ITEM [] [ TEXT " simple text " ]
        , CODE_NOTATION "code_notation"
        , ITEM [] [ TEXT " " ]
        , ITEM [ Bold ]
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
-- heading
-- italic
-- strikethrough

data ItemStyle
    = PlainItem
    | BoldItem
    | ItalicItem
    | StrikeThroughItem

newtype StyledItem (a :: ItemStyle) = StyledItem
    { getStyledItem :: [Segment]
    } deriving Show

instance Arbitrary (StyledItem a) where
    arbitrary = do
        newSize <- choose (0, sizeNum)
        scale (\size -> if size < sizeNum then size else newSize) $
            StyledItem . concatSegment . addSpace <$> listOf1 arbitrary
        where
          -- Add space after hashtag
          addSpace :: [Segment] -> [Segment]
          addSpace []                 = []
          addSpace [HASHTAG txt]      = [HASHTAG txt]
          addSpace (HASHTAG txt:rest) = HASHTAG txt : TEXT " " : addSpace rest
          addSpace (x:xs)             = x : addSpace xs

          sizeNum :: Int
          sizeNum = 10

instance ScrapboxSyntax (StyledItem 'PlainItem) where
    render (StyledItem segments)     = renderSegments segments
    getContent (StyledItem segments) = renderSegments segments

instance ScrapboxSyntax (StyledItem 'BoldItem) where
    render (StyledItem segments)     = "[* " <> renderSegments segments <> "]"
    getContent (StyledItem segments) = renderSegments segments

instance ScrapboxSyntax (StyledItem 'ItalicItem) where
    render (StyledItem segments)     = "[/ " <> renderSegments segments <> "]"
    getContent (StyledItem segments) = renderSegments segments

instance ScrapboxSyntax (StyledItem 'StrikeThroughItem) where
    render (StyledItem segments)     = "[- " <> renderSegments segments <> "]"
    getContent (StyledItem segments) = renderSegments segments

styledItemSpec :: Spec
styledItemSpec = describe "Styled inlines" $ do
    describe "Non-Styled" $ do
        prop "should parse as Non-styled" $
            \(plainInline :: StyledItem 'PlainItem) ->
                testParse plainInline null
        prop "should preserve its content" $
            \(plainInline :: StyledItem 'PlainItem) ->
                testContent plainInline

    describe "Bold" $ do
        prop "should parse as Bold" $
            \(boldInline :: StyledItem 'BoldItem) ->
                testParse
                    boldInline
                    (\styles -> length styles == 1 && all isBold styles)
        prop "should preserve its content" $
            \(boldInline :: StyledItem 'BoldItem) ->
                testContent boldInline

    describe "Italic" $ do
        prop "should parse as Bold" $
            \(italicInline :: StyledItem 'ItalicItem) ->
                testParse
                    italicInline
                    (\styles -> length styles == 1 && all isItalic styles)
        prop "should preserve its content" $
            \(italicInline :: StyledItem 'ItalicItem) ->
                testContent italicInline

    describe "StrikeThrough" $ do
        prop "should parse as StrikeThrough" $
            \(strikeThroughInline :: StyledItem 'StrikeThroughItem) ->
                testParse 
                    strikeThroughInline 
                    (\styles -> length styles == 1 && all isStrikeThrough styles)

        prop "should preserve its content" $
            \(strikeThroughInline :: StyledItem 'StrikeThroughItem) ->
                testContent strikeThroughInline
  where
    getItem :: InlineBlock -> Maybe InlineBlock
    getItem item@(ITEM _ _) = Just item
    getItem _               = Nothing

    testParse :: (ScrapboxSyntax (StyledItem a))
              => StyledItem a
              -> ([Style] -> Bool)
              -> Property
    testParse inlineBlock = checkParsed inlineBlock runScrapTextParser
        (\(ScrapText inlines) -> do
            guard $ length inlines == 1
            inline <- headMaybe inlines
            (ITEM style _) <- getItem inline
            return style
        )

    testContent :: (ScrapboxSyntax (StyledItem a)) => StyledItem a -> Property
    testContent inlineBlock = checkParsed inlineBlock runScrapTextParser
        (\(ScrapText inlines) -> do
            guard $ length inlines == 1
            inline <- headMaybe inlines
            (ITEM _ segments) <- getItem inline
            return segments
        )
        (== getStyledItem inlineBlock)
