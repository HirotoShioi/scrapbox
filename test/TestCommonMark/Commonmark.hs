{-| Test suites for commonmark parser
-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Commonmark
    ( commonmarkSpec
    ) where

import           RIO

import           Data.Char (isLetter)
import           RIO.List (zipWith)
import qualified RIO.Text as T
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Gen, Property,
                                  arbitraryPrintableChar, choose, elements,
                                  genericShrink, listOf, listOf1, oneof,
                                  suchThat, vectorOf, (===))

import           Data.Scrapbox (Block (..), CodeName (..), CodeSnippet (..),
                                InlineBlock (..), Level (..), ScrapText (..),
                                Scrapbox (..), Segment (..), Start (..),
                                Style (..), TableContent (..), TableName (..),
                                Url (..), commonmarkToNode)
import           Data.Scrapbox.Internal (genPrintableUrl, runParagraphParser,
                                         shortListOf)
import           Utils (propNonNull, shouldParseSpec)

-- | Generate random text
genNoSymbolText :: Gen Text
genNoSymbolText = fromString <$> listOf (arbitraryPrintableChar `suchThat` isLetter)

genNoSymbolText1 :: Gen Text
genNoSymbolText1 = fromString <$> listOf1 (arbitraryPrintableChar `suchThat` isLetter)

commonmarkSpec :: Spec
commonmarkSpec = describe "CommonMark parser" $ modifyMaxSuccess (const 5000) $ do
    prop "Model test" commonmarkModelTest
    shouldParseSpec runParagraphParser
    prop "should return non-empty list of blocks if the given string is non-empty" $
        propNonNull runParagraphParser id

--------------------------------------------------------------------------------
-- Commonmark model test
--------------------------------------------------------------------------------

data CommonMark
    = ParagraphSection !Text
    | HeaderText !Int !Text
    | TableSection ![Text] ![[Text]]
    | ImageSection !Text !Text
    | OrderedListBlock ![Text]
    | UnorderedListBlock ![Text]
    | BlockQuoteText !Text
    | StyledText !TestStyle !Text
    | CodeBlockSection ![Text]
    | Link !Text !Text
    | CodeNotation !Text
    deriving (Eq, Show, Generic)

data TestStyle
    = BoldStyle
    | ItalicStyle
    | NoStyles
    | StrikeThroughStyle
    deriving (Eq, Enum, Show)

instance Arbitrary TestStyle where
    arbitrary = elements [BoldStyle .. StrikeThroughStyle]

renderCommonmark :: CommonMark -> Text
renderCommonmark = \case
    ParagraphSection text       -> text
    HeaderText level text       -> T.replicate level "#" <> " " <> text
    TableSection header content -> renderTable header content
    ImageSection title someLink -> "![" <> title <> "](" <> someLink <> ")"
    OrderedListBlock list       -> T.unlines $ zipWith
        (\num someText -> tshow num <> ". " <> someText)
        ([1..] :: [Int])
        list
    UnorderedListBlock list     -> T.unlines $ map ("- " <>) list
    BlockQuoteText text         -> ">" <> text
    StyledText style text       -> case style of
        BoldStyle          -> "**" <> text <> "**"
        ItalicStyle        -> "*" <> text <> "*"
        StrikeThroughStyle -> "~~" <> text <> "~~"
        NoStyles           -> text
    CodeBlockSection codes      -> T.unlines $ ["```"] <> codes <> ["```"]
    Link name url               -> "[" <> name <> "](" <> url <> ")"
    CodeNotation notation       -> "`" <> notation <> "`"

renderTable :: [Text] -> [[Text]] -> Text
renderTable header contents = do
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

instance Arbitrary CommonMark where
    arbitrary =
        oneof
          [ ParagraphSection <$> genNoSymbolText
          , HeaderText <$> choose (-6, 6) <*> genNoSymbolText
          , BlockQuoteText <$> genNoSymbolText
          , tableGenerator
          , ImageSection <$> genNoSymbolText <*> genPrintableUrl
          , OrderedListBlock <$> listOf genNoSymbolText1
          , UnorderedListBlock <$> listOf genNoSymbolText1
          , StyledText <$> elements [BoldStyle .. StrikeThroughStyle] <*> genNoSymbolText
          , CodeBlockSection <$> listOf genNoSymbolText
          , Link <$> genNoSymbolText <*> genPrintableUrl
          , CodeNotation <$> genNoSymbolText
          ]
      where
        tableGenerator = do
            rowNum   <- choose (2,10)
            header   <- vectorOf rowNum genNoSymbolText
            contents <- shortListOf $ vectorOf rowNum genNoSymbolText
            return $ TableSection header contents
    shrink = genericShrink

toScrapbox :: CommonMark -> [Block]
toScrapbox = \case
    ParagraphSection text ->
        if T.null text
            then mempty
            else [PARAGRAPH (ScrapText [SPAN [] [TEXT text]])]
    HeaderText hlevel text ->
        let blocks | hlevel <= 0 && T.null text = mempty
                   | hlevel <= 0 = [PARAGRAPH (ScrapText [SPAN [] [TEXT text]])]
                   | T.null text = [HEADING (toLevel hlevel) mempty]
                   | otherwise = [HEADING (toLevel hlevel) [TEXT text]]
        in blocks
    TableSection header content ->
        if null header || null content
            then mempty
            else [TABLE (TableName "table") (TableContent $ [header] <> content)]
    OrderedListBlock content ->
        if null content
            then mempty
            else [BULLET_POINT (Start 1) (mkParagraphs content)]
    UnorderedListBlock content ->
        if null content
            then mempty
            else [BULLET_POINT (Start 1) (mkParagraphs content)]
    ImageSection _t url -> [THUMBNAIL (Url url)]
    StyledText style text ->
        -- Needs to take a look
        let blocks | style == BoldStyle && T.null text =
                       [PARAGRAPH (ScrapText [SPAN [] [TEXT "\n"]])]
                   | style == ItalicStyle && T.null text =
                       [ PARAGRAPH (ScrapText [SPAN [] [ TEXT "**" ]])]
                   | style == StrikeThroughStyle && T.null text =
                       [CODE_BLOCK ( CodeName "code" ) ( CodeSnippet [] )]
                   | style == NoStyles && T.null text =
                       mempty
                   | otherwise = [PARAGRAPH (ScrapText [SPAN (toStyle style) [TEXT text]])]
        in blocks
    BlockQuoteText text ->
        if T.null text
            then [BLOCK_QUOTE (ScrapText [])]
            else [BLOCK_QUOTE (ScrapText [SPAN [] [TEXT text]])]
    CodeBlockSection codeBlocks ->
        [CODE_BLOCK (CodeName "code") (CodeSnippet codeBlocks)]
    Link name url ->
        if T.null name
            then [PARAGRAPH (ScrapText [SPAN [] [LINK Nothing (Url url)]])]
            else [PARAGRAPH (ScrapText [SPAN [] [LINK (Just name) (Url url)]])]
    CodeNotation notation ->
        if T.null notation
            then [PARAGRAPH ( ScrapText [SPAN [] [ TEXT "``" ]])]
            else [PARAGRAPH (ScrapText [CODE_NOTATION notation])]
  where
    toLevel :: Int -> Level
    toLevel = \case
        1 -> Level 4
        2 -> Level 3
        3 -> Level 2
        4 -> Level 1
        _ -> Level 1

    mkParagraphs :: [Text] -> [Block]
    mkParagraphs = map toParagraph

    toParagraph c =
        let scrapText =  ScrapText [SPAN [] [TEXT c]]
        in PARAGRAPH scrapText

    toStyle :: TestStyle -> [Style]
    toStyle = \case
        BoldStyle          -> [Bold]
        ItalicStyle        -> [Italic]
        StrikeThroughStyle -> [StrikeThrough]
        NoStyles           -> []

commonmarkModelTest :: CommonMark -> Property
commonmarkModelTest commonmark =
    let (Scrapbox content) = commonmarkToNode [] . renderCommonmark $ commonmark
    in content === toScrapbox commonmark
