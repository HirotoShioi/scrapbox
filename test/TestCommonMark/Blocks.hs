{-| Test suites for testing parser on Block
-}

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Blocks
    ( blockSpec
    ) where

import           RIO

import           RIO.List
import qualified RIO.Text as T
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), choose, elements, listOf1,
                                  vectorOf)

import           Data.Scrapbox (Block (..), CodeSnippet (..), Level (..),
                                TableContent (..), Url (..))
import           Data.Scrapbox.Internal (isBlockQuote, isBulletPoint,
                                         isCodeBlock, isHeader, isParagraph,
                                         isTable, isThumbnail, renderBlock,
                                         renderSegments, renderText)
import           TestCommonMark.Utils (CommonMark (..), checkScrapbox,
                                       getParagraph)
import           Utils (genPrintableText, genPrintableUrl, genText)

-- | Test suites for 'Block'
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
-- Paragraph
--------------------------------------------------------------------------------

-- | Paragraph section
newtype ParagraphSection = ParagraphSection
    { getParagraphSection :: Text
    } deriving Show

instance CommonMark ParagraphSection where
    render (ParagraphSection txt) = txt

instance Arbitrary ParagraphSection where
    arbitrary = ParagraphSection <$> genPrintableText

-- | Test spec for parsing 'PARAGRAPH'
paragraphSpec :: Spec
paragraphSpec = describe "Paragraph" $ do
    prop "should be able to parse paragraph as PARAGRAPH" $
        \(paragraph :: ParagraphSection) ->
            checkScrapbox paragraph isParagraph headMaybe

    prop "should preserve its content" $
        \(paragraph :: ParagraphSection) ->
            checkScrapbox paragraph
                (\paragraphText -> paragraphText == getParagraphSection paragraph)
                (\content -> do
                    blockContent 　　　<- headMaybe content
                    (PARAGRAPH stext) <- getParagraph blockContent
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

instance CommonMark HeaderText where
    render = \case
        H1 textContent -> "# " <> textContent
        H2 textContent -> "## " <> textContent
        H3 textContent -> "### " <> textContent
        H4 textContent -> "#### " <> textContent
        H5 textContent -> "##### " <> textContent
        H6 textContent -> "###### " <> textContent

-- | Test spec for parsing 'HEADING' text
headerTextSpec :: Spec
headerTextSpec = describe "Header text" $ do
    prop "should be able to parse header text as HEADING" $
        \(headerText :: HeaderText) ->
            checkScrapbox headerText isHeader headMaybe

    prop "should preserve header size" $
        \(headerText :: HeaderText) ->
            checkScrapbox headerText
                (`isSameLevel` headerText)
                (\content -> do
                    blockContent          <- headMaybe content
                    (HEADING level _) <- getHeader blockContent
                    return level
                )
    prop "should preserve its content" $
        \(headerText :: HeaderText) ->
            checkScrapbox headerText
                (\headerContent -> headerContent == getHeaderTextContent headerText)
                (\content -> do
                    blockContent             <- headMaybe content
                    (HEADING _ headerContent) <- getHeader blockContent
                    return $ renderSegments headerContent
                )
  where
    getHeader :: Block -> Maybe Block
    getHeader header@(HEADING _ _) = Just header
    getHeader _                    = Nothing

    -- Check if given headerSize is same size
    isSameLevel :: Level -> HeaderText -> Bool
    isSameLevel (Level 4) (H1 _) = True
    isSameLevel (Level 3) (H2 _) = True
    isSameLevel (Level 2) (H3 _) = True
    isSameLevel (Level 1) (H4 _) = True
    isSameLevel (Level 1) (H5 _) = True
    isSameLevel (Level 1) (H6 _) = True
    isSameLevel _ _              = False

--------------------------------------------------------------------------------
-- BlockQuote
--------------------------------------------------------------------------------

-- | Blockquote
newtype BlockQuoteText = BlockQuoteText
    { getBlockQuoteText :: Text
    } deriving Show

instance CommonMark BlockQuoteText where
    render (BlockQuoteText txt) = ">" <> txt

instance Arbitrary BlockQuoteText where
    arbitrary = BlockQuoteText <$> genPrintableText

-- | Test spec for parsing 'BLOCK_QUOTE'
blockQuoteSpec :: Spec
blockQuoteSpec = describe "BlockQuote text" $ do
    prop "should be able parse block quote text as BLOCK_QUOTE" $
        \(blockQuote :: BlockQuoteText) ->
            checkScrapbox blockQuote isBlockQuote headMaybe

    prop "should preserve its content" $
        \(blockQuote :: BlockQuoteText) ->
            checkScrapbox blockQuote
                (\quoteText -> quoteText == getBlockQuoteText blockQuote)
                (\content -> do
                    blockContent       <- headMaybe content
                    (BLOCK_QUOTE stext) <- getBlockQuote blockContent
                    return $ renderText stext
                )
  where
    -- Should this function be here?
    getBlockQuote :: Block -> Maybe Block
    getBlockQuote blockQuote@(BLOCK_QUOTE _) = Just blockQuote
    getBlockQuote _                          = Nothing

--------------------------------------------------------------------------------
-- CodeBlock
--------------------------------------------------------------------------------

-- | Codeblock section
newtype CodeBlockSection = CodeBlockSection
    { getCodeBlockContent :: [Text]
    } deriving Show

instance CommonMark CodeBlockSection where
    render (CodeBlockSection codes) = T.unlines $ ["```"] <> codes <> ["```"]

instance Arbitrary CodeBlockSection where
    arbitrary = CodeBlockSection <$> listOf1 genPrintableText

-- | Test spec for parsing 'CODE_BLOCK'
codeBlockSpec :: Spec
codeBlockSpec = describe "Code block" $ do
    prop "should parse code block content as CODE_BLOCK" $
        \(codeBlock :: CodeBlockSection) ->
            checkScrapbox codeBlock isCodeBlock headMaybe
    prop "should preserve its content" $
        \(codeBlock :: CodeBlockSection) ->
            checkScrapbox codeBlock
                (\codeContent -> codeContent == getCodeBlockContent codeBlock)
                (\content -> do
                    blockContent <- headMaybe content
                    (CODE_BLOCK _ (CodeSnippet snippet)) <- getCodeBlock blockContent
                    return snippet
                )
  where
    getCodeBlock :: Block -> Maybe Block
    getCodeBlock codeBlock@(CODE_BLOCK _ _) = Just codeBlock
    getCodeBlock _                          = Nothing

--------------------------------------------------------------------------------
-- Unordered list
--------------------------------------------------------------------------------

-- | Unordered list
newtype UnorderedListBlock = UnorderedListBlock
    { getUnorderedListBlock :: [Text]
    } deriving Show

instance CommonMark UnorderedListBlock where
    render (UnorderedListBlock list) = T.unlines $ map ("- " <>) list

instance Arbitrary UnorderedListBlock where
    arbitrary = UnorderedListBlock <$> listOf1 genPrintableText

-- | Test spec for parsing unordered list
unorderedListSpec :: Spec
unorderedListSpec = describe "Unordered list" $ do
    prop "should parse unordered list as BULLET_POINT" $
        \(unorderedListBlock :: UnorderedListBlock) ->
            checkScrapbox unorderedListBlock isBulletPoint headMaybe

    prop "should preserve its content" $
        \(unorderedListBlock :: UnorderedListBlock) ->
            checkScrapbox unorderedListBlock
                (\renderedTexts ->
                    renderedTexts == getUnorderedListBlock unorderedListBlock
                )
                (\content -> do
                    blockContent          <- headMaybe content
                    (BULLET_POINT _ lists) <- getBulletPoint blockContent
                    return $ concatMap renderBlock lists
                )

-- | Check whether given 'Block' is 'BULLET_POINT'
getBulletPoint :: Block -> Maybe Block
getBulletPoint bulletList@(BULLET_POINT _ _) = Just bulletList
getBulletPoint _                             = Nothing

--------------------------------------------------------------------------------
-- Ordered list
--------------------------------------------------------------------------------

-- | OrderedList
newtype OrderedListBlock = OrderedListBlock
    { getOrderedListBlock :: [Text]
    } deriving Show

instance Arbitrary OrderedListBlock where
    arbitrary = OrderedListBlock <$> listOf1 genPrintableText

instance CommonMark OrderedListBlock where
    render (OrderedListBlock list) = T.unlines $
        zipWith
            (\num someText -> tshow num <> ". " <> someText)
            ([1..] :: [Int])
            list

-- | Test spec for parsing Ordered list
orderedListSpec :: Spec
orderedListSpec = describe "Ordered list" $ do
    prop "should parse ordered list as BULLET_POINT" $
        \(orderedListBlock :: OrderedListBlock) ->
            checkScrapbox orderedListBlock isBulletPoint headMaybe

    prop "should preserve its content" $
        \(orderedListBlock :: OrderedListBlock) ->
            checkScrapbox orderedListBlock
                (\renderedTexts ->
                    renderedTexts == getOrderedListBlock orderedListBlock
                )
                (\content -> do
                    blockContent        <- headMaybe content
                    (BULLET_POINT _ lists) <- getBulletPoint blockContent
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

instance CommonMark ImageSection where
    render (ImageSection title someLink) = "![" <> title <> "](" <> someLink <> ")"

instance Arbitrary ImageSection where
    arbitrary = ImageSection <$> genText <*> genPrintableUrl

-- | Test spec for parsing image
imageSpec :: Spec
imageSpec =
    describe "Image" $ do
        prop "should parse image as THUMBNAIL" $
            \(imageSection :: ImageSection) ->
                checkScrapbox imageSection isThumbnail headMaybe
        prop "should preserve its link" $
            \(imageSection :: ImageSection) ->
                checkScrapbox imageSection
                    (\(Url url) -> url == imageLink imageSection)
                    (\content -> do
                        blockContent    <- headMaybe content
                        (THUMBNAIL url) <- getThumbnail blockContent
                        return url
                    )
  where
    getThumbnail :: Block -> Maybe Block
    getThumbnail thumbnail@(THUMBNAIL _) = Just thumbnail
    getThumbnail _                       = Nothing

--------------------------------------------------------------------------------
-- Table
--------------------------------------------------------------------------------

-- | Table section
data TableSection = TableSection
    { tableHeader  :: ![Text]
    -- ^ Table header
    , tableContent :: ![[Text]]
    -- ^ Content of the table
    } deriving Show

instance CommonMark TableSection where
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
        header   <- vectorOf rowNum genText
        contents <- listOf1 $ vectorOf rowNum genText
        return $ TableSection header contents

-- | Test spec for parsing table
tableSpec :: Spec
tableSpec = describe "Table" $ do
    prop "should parse tabel as TABLE" $
        \(table :: TableSection) -> checkScrapbox table isTable headMaybe
    prop "should preserve its content" $
        \(table :: TableSection) ->
            checkScrapbox table
                (\(TableContent contents) ->
                  contents == [tableHeader table] <> tableContent table
                )
                (\content -> do
                    blockContent     <- headMaybe content
                    (TABLE _ tables) <- getTable blockContent
                    return tables
                )
  where
    getTable :: Block -> Maybe Block
    getTable table@(TABLE _ _) = Just table
    getTable _                 = Nothing
