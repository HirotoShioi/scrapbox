{-| Test suites for testing parser on Block
-}

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Blocks where

import           RIO

import           RIO.List (headMaybe, zipWith)
import qualified RIO.Text as T
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), choose, elements, listOf1,
                                  vectorOf, (===))

import           Data.Scrapbox (Block (..), CodeName (..), CodeSnippet (..),
                                CodeSnippet (..), InlineBlock (..), Level (..),
                                ScrapText (..), Segment (..), Start (..),
                                Style (..), TableContent (..), TableName (..),
                                Url (..))
import           TestCommonMark.Utils (checkScrapbox)
import           Utils (Syntax (..), genNoSymbolText, genPrintableUrl)

-- | Test suites for 'Block'
blockSpec :: Spec
blockSpec = describe "Block" $ modifyMaxSuccess (const 1000) $
    describe "should preserve its content" $ do
        -- Blocks
        paragraphSpec
        headerTextSpec
        blockQuoteSpec
        codeBlockSpec
        unorderedListSpec
        orderedListSpec
        imageSpec
        tableSpec
        styleSpec

--------------------------------------------------------------------------------
-- Paragraph
--------------------------------------------------------------------------------

-- | Paragraph section
newtype ParagraphSection = ParagraphSection Text
    deriving Show

instance Syntax ParagraphSection where
    render (ParagraphSection txt) = txt

instance Arbitrary ParagraphSection where
    arbitrary = ParagraphSection <$> genNoSymbolText

-- | Test spec for parsing 'PARAGRAPH'
paragraphSpec :: Spec
paragraphSpec = prop "Paragraph" $
    \paragraph@(ParagraphSection txt) ->
        checkScrapbox paragraph
            (=== PARAGRAPH (ScrapText [SPAN [] [TEXT txt]]))
            headMaybe

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Header text
data HeaderText = HeaderText !Int !Text
    deriving Show

instance Arbitrary HeaderText where
    arbitrary = do
        randomSize <- choose (1, 6)
        someText   <- genNoSymbolText
        return $ HeaderText randomSize someText

instance Syntax HeaderText where
    render (HeaderText size txt) =  T.replicate size "#" <> " " <> txt

-- | Test spec for parsing 'HEADING' text
headerTextSpec :: Spec
headerTextSpec = prop "should preserve its content" $
    \headerText@(HeaderText size txt) ->
        checkScrapbox headerText
            (=== HEADING (toLevel size) [TEXT txt])
            headMaybe
  where
    toLevel :: Int -> Level
    toLevel 1 = Level 4
    toLevel 2 = Level 3
    toLevel 3 = Level 2
    toLevel 4 = Level 1
    toLevel _ = Level 1

--------------------------------------------------------------------------------
-- BlockQuote
--------------------------------------------------------------------------------

-- | Blockquote
newtype BlockQuoteText = BlockQuoteText Text
    deriving Show

instance Syntax BlockQuoteText where
    render (BlockQuoteText txt) = ">" <> txt

instance Arbitrary BlockQuoteText where
    arbitrary = BlockQuoteText <$> genNoSymbolText

-- | Test spec for parsing 'BLOCK_QUOTE'
blockQuoteSpec :: Spec
blockQuoteSpec = prop "BlockQuote text" $ \blockQuote@(BlockQuoteText text) ->
    checkScrapbox blockQuote
        (=== BLOCK_QUOTE (ScrapText [SPAN [] [TEXT text]]))
        headMaybe

--------------------------------------------------------------------------------
-- CodeBlock
--------------------------------------------------------------------------------

-- | Codeblock section
newtype CodeBlockSection = CodeBlockSection [Text]
    deriving Show

instance Syntax CodeBlockSection where
    render (CodeBlockSection codes) = T.unlines $ ["```"] <> codes <> ["```"]

instance Arbitrary CodeBlockSection where
    arbitrary = CodeBlockSection <$> listOf1 genNoSymbolText

-- | Test spec for parsing 'CODE_BLOCK'
codeBlockSpec :: Spec
codeBlockSpec = prop "Code block" $
    \codeBlock@(CodeBlockSection codeBlocks) ->
        checkScrapbox codeBlock
            (=== CODE_BLOCK (CodeName "code") (CodeSnippet codeBlocks))
            headMaybe

--------------------------------------------------------------------------------
-- Unordered list
--------------------------------------------------------------------------------

-- | Unordered list
newtype UnorderedListBlock = UnorderedListBlock [Text]
    deriving Show

instance Syntax UnorderedListBlock where
    render (UnorderedListBlock list) = T.unlines $ map ("- " <>) list

instance Arbitrary UnorderedListBlock where
    arbitrary = UnorderedListBlock <$> listOf1 genNoSymbolText

-- | Test spec for parsing unordered list
unorderedListSpec :: Spec
unorderedListSpec = prop "Unordered list" $
    \unorderedListBlock@(UnorderedListBlock content) ->
        checkScrapbox unorderedListBlock
            (=== BULLET_POINT (Start 1) (mkParagraphs content))
            headMaybe

mkParagraphs :: [Text] -> [Block]
mkParagraphs = map toParagraph
  where
    toParagraph c =
        let scrapText =  ScrapText [SPAN [] [TEXT c]]
        in PARAGRAPH scrapText

--------------------------------------------------------------------------------
-- Ordered list
--------------------------------------------------------------------------------

-- | OrderedList
newtype OrderedListBlock = OrderedListBlock [Text]
    deriving Show

instance Arbitrary OrderedListBlock where
    arbitrary = OrderedListBlock <$> listOf1 genNoSymbolText

instance Syntax OrderedListBlock where
    render (OrderedListBlock list) = T.unlines $
        zipWith
            (\num someText -> tshow num <> ". " <> someText)
            ([1..] :: [Int])
            list

-- | Test spec for parsing Ordered list
orderedListSpec :: Spec
orderedListSpec = prop "Ordered list" $
    \orderedListBlock@(OrderedListBlock content) ->
        checkScrapbox orderedListBlock
            (=== BULLET_POINT (Start 1) (mkParagraphs content))
            headMaybe

--------------------------------------------------------------------------------
-- Images
--------------------------------------------------------------------------------

-- | Image section
data ImageSection = ImageSection !Text !Text
    deriving Show

instance Syntax ImageSection where
    render (ImageSection title someLink) = "![" <> title <> "](" <> someLink <> ")"

instance Arbitrary ImageSection where
    arbitrary = ImageSection <$> genNoSymbolText <*> genPrintableUrl

-- | Test spec for parsing image
imageSpec :: Spec
imageSpec = prop "Image" $
    \imageSection@(ImageSection _t url) ->
        checkScrapbox imageSection
            (=== THUMBNAIL (Url url))
            headMaybe

--------------------------------------------------------------------------------
-- Table
--------------------------------------------------------------------------------

-- | Table section
data TableSection = TableSection ![Text] ![[Text]]
    deriving Show

instance Syntax TableSection where
    render (TableSection header contents) = do
        let renderedHeader   = renderColumn header
        let between          = renderBetween' (length header)
        let renderedContents = renderTableContent contents
        T.unlines $ [renderedHeader] <> [between] <> renderedContents
      where
        renderColumn :: [Text] -> Text
        renderColumn = foldl' (\acc a -> acc <> a <> " | ") "| "

        renderBetween' :: Int -> Text
        renderBetween' rowNum' = T.replicate rowNum' "|- " <> "|"

        renderTableContent :: [[Text]] -> [Text]
        renderTableContent = map renderColumn

instance Arbitrary TableSection where
    arbitrary = do
        rowNum   <- choose (2,10)
        header   <- vectorOf rowNum genNoSymbolText
        contents <- listOf1 $ vectorOf rowNum genNoSymbolText
        return $ TableSection header contents

-- | Test spec for parsing table
tableSpec :: Spec
tableSpec = prop "Table" $
    \table@(TableSection header content) ->
        checkScrapbox table
            (=== TABLE
                (TableName "table")
                (TableContent $ [header] <> content)
            )
            headMaybe

--------------------------------------------------------------------------------
-- Styled text
--------------------------------------------------------------------------------
-- | Use Phantom type so we can generalize the test
data StyledText = StyledText !TestStyle !Text
    deriving Show

-- | Style type
--
-- Data constructors will be promoted using DataKinds
data TestStyle =
      BoldStyle
    | ItalicStyle
    | NoStyles
    | StrikeThroughStyle
    deriving (Show)

instance Syntax StyledText where
    render (StyledText style text) = case style of
        BoldStyle          -> "**" <> text <> "**"
        ItalicStyle        -> "*" <> text <> "*"
        StrikeThroughStyle -> "~~" <> text <> "~~"
        NoStyles           -> text

instance Arbitrary StyledText where
    arbitrary = do
        randomStyle <- elements [BoldStyle, ItalicStyle, NoStyles, StrikeThroughStyle]
        randomText  <- genNoSymbolText
        return $ StyledText randomStyle randomText

-- | Test suites for parsing styled text
styleSpec :: Spec
styleSpec = prop "Styles" $
    \styledText@(StyledText style text) ->
        checkScrapbox styledText
            (=== PARAGRAPH (ScrapText [SPAN (toStyle style) [TEXT text]]))
            headMaybe
    where
        toStyle :: TestStyle -> [Style]
        toStyle = \case
            BoldStyle          -> [Bold]
            ItalicStyle        -> [Italic]
            StrikeThroughStyle -> [StrikeThrough]
            NoStyles           -> []
