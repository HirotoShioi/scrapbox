{-| Test suites for commonmark parser
-}

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Commonmark
    ( commonmarkSpec
    ) where

import           RIO
import           RIO.List (headMaybe, zipWith)
import qualified RIO.Text as T
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Property, choose, elements,
                                  listOf1, oneof, property, vectorOf, (===))

import           Data.Scrapbox (Block (..), CodeName (..), CodeSnippet (..),
                                InlineBlock (..), Level (..), ScrapText (..),
                                Scrapbox (..), Segment (..), Start (..),
                                Style (..), TableContent (..), TableName (..),
                                Url (..), commonmarkToNode)
import           Data.Scrapbox.Internal (genPrintableUrl, runParagraphParser,
                                         shortListOf)
import           Utils (genNoSymbolText, propNonNull, shouldParseSpec)

commonmarkSpec :: Spec
commonmarkSpec = describe "CommonMark parser" $ modifyMaxSuccess (const 200) $ do
    describe "Block" $ modifyMaxSuccess (const 5000) $
        prop "should preserve its content" commomMarkRoundTripTest

    describe "runParagraphParser" $ modifyMaxSuccess (const 5000) $
        shouldParseSpec runParagraphParser

    prop "should return non-empty list of blocks if the given string is non-empty" $
        propNonNull runParagraphParser id

data CommonMark =
      ParagraphSection !Text
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
    deriving Show

data TestStyle =
      BoldStyle
    | ItalicStyle
    | NoStyles
    | StrikeThroughStyle
    deriving (Show, Enum)

renderTestData :: CommonMark -> Text
renderTestData = \case
    ParagraphSection text -> text
    HeaderText level text  -> T.replicate level "#" <> " " <> text
    TableSection header content -> renderTable header content
    ImageSection title someLink -> "![" <> title <> "](" <> someLink <> ")"
    OrderedListBlock list -> T.unlines $ zipWith
            (\num someText -> tshow num <> ". " <> someText)
            ([1..] :: [Int])
            list
    UnorderedListBlock list -> T.unlines $ map ("- " <>) list
    BlockQuoteText text    -> ">" <> text
    StyledText style text  -> case style of
        BoldStyle          -> "**" <> text <> "**"
        ItalicStyle        -> "*" <> text <> "*"
        StrikeThroughStyle -> "~~" <> text <> "~~"
        NoStyles           -> text
    CodeBlockSection codes -> T.unlines $ ["```"] <> codes <> ["```"]
    Link name url          -> "[" <> name <> "](" <> url <> ")"
    CodeNotation notation  -> "`" <> notation <> "`"

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
          , HeaderText <$> choose (1,6) <*> genNoSymbolText
          , BlockQuoteText <$> genNoSymbolText
          , tableGenerator
          , ImageSection <$> genNoSymbolText <*> genPrintableUrl
          , OrderedListBlock <$> listOf1 genNoSymbolText
          , UnorderedListBlock <$> listOf1 genNoSymbolText
          , StyledText <$> elements [BoldStyle .. StrikeThroughStyle] <*> genNoSymbolText
          , CodeBlockSection <$> listOf1 genNoSymbolText
          , Link <$> genNoSymbolText <*> genPrintableUrl
          , CodeNotation <$> genNoSymbolText
          ]
      where
        tableGenerator = do
            rowNum   <- choose (2,10)
            header   <- vectorOf rowNum genNoSymbolText
            contents <- shortListOf $ vectorOf rowNum genNoSymbolText
            return $ TableSection header contents

toModel :: CommonMark -> Block
toModel = \case
    ParagraphSection text       -> PARAGRAPH (ScrapText [SPAN [] [TEXT text]])
    HeaderText hlevel text      -> HEADING (toLevel hlevel) [TEXT text]
    TableSection header content -> TABLE (TableName "table") (TableContent $ [header] <> content)
    OrderedListBlock content    -> BULLET_POINT (Start 1) (mkParagraphs content)
    UnorderedListBlock content  -> BULLET_POINT (Start 1) (mkParagraphs content)
    ImageSection _t url         -> THUMBNAIL (Url url)
    StyledText style text       -> PARAGRAPH (ScrapText [SPAN (toStyle style) [TEXT text]])
    BlockQuoteText text         -> BLOCK_QUOTE (ScrapText [SPAN [] [TEXT text]])
    CodeBlockSection codeBlocks -> CODE_BLOCK (CodeName "code") (CodeSnippet codeBlocks)
    Link name url               -> PARAGRAPH (ScrapText [SPAN [] [LINK (Just name) (Url url)]])
    CodeNotation notation       -> PARAGRAPH (ScrapText [CODE_NOTATION notation])
  where
    toLevel :: Int -> Level
    toLevel 1 = Level 4
    toLevel 2 = Level 3
    toLevel 3 = Level 2
    toLevel 4 = Level 1
    toLevel _ = Level 1

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

commomMarkRoundTripTest :: CommonMark -> Property
commomMarkRoundTripTest commonMark =
    let (Scrapbox content) = commonmarkToNode [] . renderTestData $ commonMark
    in maybe (property False) (=== toModel commonMark) (headMaybe content)
