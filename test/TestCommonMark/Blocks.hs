{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Blocks where

import           RIO

import           RIO.List
import qualified RIO.Text              as T
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), choose, elements,
                                        listOf1, vectorOf)

import           Render                (renderBlock, renderContent, renderText)
import           Types                 (Block (..), CodeSnippet (..),
                                        HeaderSize (..), TableContent (..),
                                        Url (..), isBlockQuote, isBulletList,
                                        isCodeBlock, isHeader, isParagraph,
                                        isTable, isThumbnail)

import           TestCommonMark.Utils  (CommonMarkdown (..), checkMarkdown,
                                        genPrintableText, genPrintableUrl,
                                        genRandomText, getParagraph)

--------------------------------------------------------------------------------
-- Paragraph
--------------------------------------------------------------------------------

-- | Paragraph section
newtype ParagraphSection = ParagraphSection {
    getParagraphSection :: Text
    } deriving Show

instance CommonMarkdown ParagraphSection where
    render (ParagraphSection txt) = txt

instance Arbitrary ParagraphSection where
    arbitrary = ParagraphSection <$> genPrintableText

paragraphSpec :: Spec
paragraphSpec = describe "Paragraph" $ do
    prop "should be able to parse paragraph as Paragraph" $
        \(paragraph :: ParagraphSection) ->
            checkMarkdown paragraph isParagraph headMaybe

    prop "should preserve its content" $
        \(paragraph :: ParagraphSection) ->
            checkMarkdown paragraph
                (\paragraphText -> paragraphText == getParagraphSection paragraph)
                (\content -> do
                    blockContent 　　　<- headMaybe content
                    (Paragraph stext) <- getParagraph blockContent
                    return $ renderText stext
                )

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Header text
data HeaderText
    = H1 Text
    | H2 Text
    | H3 Text
    | H4 Text
    | H5 Text
    | H6 Text
    deriving Show

-- | Get the content of the 'HeaderText'
getHeaderTextContent :: HeaderText -> Text
getHeaderTextContent = \case
    H1 txt -> txt
    H2 txt -> txt
    H3 txt -> txt
    H4 txt -> txt
    H5 txt -> txt
    H6 txt -> txt

instance Arbitrary HeaderText where
    arbitrary = do
        someText <- genPrintableText
        elements
            [ H1 someText
            , H2 someText
            , H3 someText
            , H4 someText
            , H5 someText
            , H6 someText
            ]

instance CommonMarkdown HeaderText where
    render = \case
        H1 textContent -> "# " <> textContent
        H2 textContent -> "## " <> textContent
        H3 textContent -> "### " <> textContent
        H4 textContent -> "#### " <> textContent
        H5 textContent -> "##### " <> textContent
        H6 textContent -> "###### " <> textContent

-- | Test spec for Header text
headerTextSpec :: Spec
headerTextSpec = describe "Header text" $ do
    prop "should be able to parse header text as Header" $
        \(headerText :: HeaderText) ->
            checkMarkdown headerText isHeader headMaybe

    prop "should preserve header size" $
        \(headerText :: HeaderText) ->
            checkMarkdown headerText
                (`isSameHeaderSize` headerText)
                (\content -> do
                    blockContent          <- headMaybe content
                    (Header headerSize _) <- getHeader blockContent
                    return headerSize
                )
    prop "should preserve its content" $
        \(headerText :: HeaderText) ->
            checkMarkdown headerText
                (\headerContent -> headerContent == getHeaderTextContent headerText)
                (\content -> do
                    blockContent             <- headMaybe content
                    (Header _ headerContent) <- getHeader blockContent
                    return $ renderContent headerContent
                )
  where
    getHeader :: Block -> Maybe Block
    getHeader header@(Header _ _) = Just header
    getHeader _                   = Nothing

    -- Check if given headerSize is same size
    isSameHeaderSize :: HeaderSize -> HeaderText -> Bool
    isSameHeaderSize (HeaderSize 4) (H1 _) = True
    isSameHeaderSize (HeaderSize 3) (H2 _) = True
    isSameHeaderSize (HeaderSize 2) (H3 _) = True
    isSameHeaderSize (HeaderSize 1) (H4 _) = True
    isSameHeaderSize (HeaderSize 1) (H5 _) = True
    isSameHeaderSize (HeaderSize 1) (H6 _) = True
    isSameHeaderSize _ _                   = False

--------------------------------------------------------------------------------
-- BlockQuote
--------------------------------------------------------------------------------

-- | Blockquote
newtype BlockQuoteText = BlockQuoteText
    { getBlockQuoteText :: Text
    } deriving Show

instance CommonMarkdown BlockQuoteText where
    render (BlockQuoteText txt) = ">" <> txt

instance Arbitrary BlockQuoteText where
    arbitrary = BlockQuoteText <$> genPrintableText

blockQuoteSpec :: Spec
blockQuoteSpec = describe "BlockQuote text" $ do
    prop "should be able parse block quote text as BlockQuote" $
        \(blockQuote :: BlockQuoteText) ->
            checkMarkdown blockQuote isBlockQuote headMaybe

    prop "should preserve its content" $
        \(blockQuote :: BlockQuoteText) ->
            checkMarkdown blockQuote
                (\quoteText -> quoteText == getBlockQuoteText blockQuote)
                (\content -> do
                    blockContent       <- headMaybe content
                    (BlockQuote stext) <- getBlockQuote blockContent
                    return $ renderText stext
                )
  where
    -- Should this function be here?
    getBlockQuote :: Block -> Maybe Block
    getBlockQuote blockQuote@(BlockQuote _) = Just blockQuote
    getBlockQuote _                         = Nothing

blockSpec :: Spec
blockSpec = describe "Block" $ do
    -- Blocks
    paragraphSpec
    headerTextSpec
    blockQuoteSpec
    codeBlockSpec
    unorderedListSpec
    orderedListSpec
    imageSpec
    tableSpec

--------------------------------------------------------------------------------
-- CodeBlock
--------------------------------------------------------------------------------

-- | Codeblock section
newtype CodeBlockSection = CodeBlockSection
    { getCodeBlockContent :: [Text]
    } deriving Show

instance CommonMarkdown CodeBlockSection where
    render (CodeBlockSection codes) = T.unlines $ ["```"] <> codes <> ["```"]

instance Arbitrary CodeBlockSection where
    arbitrary = CodeBlockSection <$> listOf1 genPrintableText

codeBlockSpec :: Spec
codeBlockSpec = describe "Code block" $ do
    prop "should parse code block content as CodeBlock" $
        \(codeBlock :: CodeBlockSection) ->
            checkMarkdown codeBlock isCodeBlock headMaybe
    prop "should preserve its content" $
        \(codeBlock :: CodeBlockSection) ->
            checkMarkdown codeBlock
                (\codeContent -> codeContent == T.unlines (getCodeBlockContent codeBlock))
                (\content -> do
                    blockContent <- headMaybe content
                    (CodeBlock _ (CodeSnippet snippet)) <- getCodeBlock blockContent
                    return snippet
                )
  where
    getCodeBlock :: Block -> Maybe Block
    getCodeBlock codeBlock@(CodeBlock _ _) = Just codeBlock
    getCodeBlock _                         = Nothing

--------------------------------------------------------------------------------
-- Unordered list
--------------------------------------------------------------------------------

-- | Unordered list
newtype UnorderedListBlock = UnorderedListBlock
    { getUnorderedListBlock :: [Text]
    } deriving Show

instance CommonMarkdown UnorderedListBlock where
    render (UnorderedListBlock list) = T.unlines $ map (\element -> "- " <> element) list

instance Arbitrary UnorderedListBlock where
    arbitrary = UnorderedListBlock <$> listOf1 genPrintableText

unorderedListSpec :: Spec
unorderedListSpec = describe "Unordered list" $ do
    prop "should parse unordered list as BulletList" $
        \(unorderedListBlock :: UnorderedListBlock) ->
            checkMarkdown unorderedListBlock isBulletList headMaybe

    prop "should preserve its content" $
        \(unorderedListBlock :: UnorderedListBlock) ->
            checkMarkdown unorderedListBlock
                (\renderedTexts -> renderedTexts == getUnorderedListBlock unorderedListBlock)
                (\content -> do
                    blockContent       <- headMaybe content
                    (BulletList lists) <- getBulletList blockContent
                    return $ concatMap renderBlock lists
                )

getBulletList :: Block -> Maybe Block
getBulletList bulletList@(BulletList _) = Just bulletList
getBulletList _                         = Nothing

--------------------------------------------------------------------------------
-- Ordered list
--------------------------------------------------------------------------------

-- | OrderedList
newtype OrderedListBlock = OrderedListBlock
    { getOrderedListBlock :: [Text]
    } deriving Show

instance Arbitrary OrderedListBlock where
    arbitrary = OrderedListBlock <$> listOf1 genPrintableText

instance CommonMarkdown OrderedListBlock where
    render (OrderedListBlock list) = T.unlines $
        zipWith (\num someText -> tshow num <> ". " <> someText) ([1..] :: [Int]) list

orderedListSpec :: Spec
orderedListSpec = describe "Ordered list" $ do
    prop "should parse ordered list as BulletList" $
        \(orderedListBlock :: OrderedListBlock) ->
            checkMarkdown orderedListBlock isBulletList headMaybe

    prop "should preserve its content" $
        \(orderedListBlock :: OrderedListBlock) ->
            checkMarkdown orderedListBlock
                (\renderedTexts -> renderedTexts == getOrderedListBlock orderedListBlock)
                (\content -> do
                    blockContent       <- headMaybe content
                    (BulletList lists) <- getBulletList blockContent
                    return $ concatMap renderBlock lists
                )

--------------------------------------------------------------------------------
-- Images
--------------------------------------------------------------------------------

-- | Image section
data ImageSection = ImageSection
    { imageTitle :: !Text
    , imageLink  :: !ImageLink
    } deriving Show

type ImageLink = Text

instance CommonMarkdown ImageSection where
    render (ImageSection title someLink) = "![" <> title <> "](" <> someLink <> ")"

instance Arbitrary ImageSection where
    arbitrary = ImageSection <$> genRandomText <*> genPrintableUrl

imageSpec :: Spec
imageSpec =
    describe "Image" $ do
        prop "should parse image as Image" $
            \(imageSection :: ImageSection) ->
                checkMarkdown imageSection isThumbnail headMaybe
        prop "should preserve its link" $
            \(imageSection :: ImageSection) ->
                checkMarkdown imageSection
                    (\(Url url) -> url == imageLink imageSection)
                    (\content -> do
                        blockContent    <- headMaybe content
                        (Thumbnail url) <- getImage blockContent
                        return url
                    )
  where
    getImage :: Block -> Maybe Block
    getImage thumbnail@(Thumbnail _) = Just thumbnail
    getImage _                       = Nothing

--------------------------------------------------------------------------------
-- Table
--------------------------------------------------------------------------------

-- | Table section
data TableSection = TableSection
    { tableHeader  :: ![Text]
    , tableContent :: ![[Text]]
    } deriving Show

instance CommonMarkdown TableSection where
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
        header   <- vectorOf rowNum genRandomText
        contents <- listOf1 $ vectorOf rowNum genRandomText
        return $ TableSection header contents

tableSpec :: Spec
tableSpec = describe "Table" $ do
    prop "should parse tabel as Table" $
        \(table :: TableSection) -> checkMarkdown table isTable headMaybe
    prop "should preserve its content" $
        \(table :: TableSection) ->
            checkMarkdown table
                (\(TableContent contents) -> contents == [tableHeader table] <> tableContent table)
                (\content -> do
                    blockContent     <- headMaybe content
                    (Table _ tables) <- getTable blockContent
                    return tables
                )
  where
    getTable :: Block -> Maybe Block
    getTable table@(Table _ _) = Just table
    getTable _                 = Nothing
