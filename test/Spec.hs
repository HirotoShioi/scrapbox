{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           RIO
import           RIO.List              (headMaybe, zipWith)
import qualified RIO.Text              as T

import           Test.Hspec            (Spec, describe, hspec)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, choose, elements,
                                        listOf1, vectorOf)

import           CommonMark.Lib        (commonmarkToMarkdown, optDefault)
import           Render                (renderBlock, renderContent, renderText)
import           Types                 (Block (..), CodeSnippet (..),
                                        Context (..), HeaderSize (..),
                                        Markdown (..), ScrapText (..),
                                        Segment (..), TableContent (..),
                                        Url (..), isBlockQuote, isBulletList,
                                        isCodeBlock, isCodeNotation, isHeader,
                                        isLink, isParagraph, isTable,
                                        isThumbnail, isSimpleText)

main :: IO ()
main = hspec $ do
    commonMarkSpec

commonMarkSpec :: Spec
commonMarkSpec = describe "CommonMark parser" $ modifyMaxSuccess (const 200) $ do
    -- Blocks
    paragraphSpec
    headerTextSpec
    blockQuoteSpec
    codeBlockSpec
    unorderedListSpec
    orderedListSpec
    imageSpec
    tableSpec

    -- Segments
    linkSpec
    codeNotationSpec
    plainTextSpec

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
        \(paragraph :: ParagraphSection) -> do
            checkMarkdown paragraph isParagraph headMaybe

    prop "should preserve its content" $
        \(paragraph :: ParagraphSection) -> do
            checkMarkdown paragraph
                (\paragraphText -> paragraphText == (getParagraphSection paragraph))
                (\content -> do
                    blockContent 　　　<- headMaybe content
                    (Paragraph stext) <- getParagraph blockContent
                    return $ renderText stext
                )

getParagraph :: Block -> Maybe Block
getParagraph paragraph@(Paragraph _) = Just paragraph
getParagraph _                       = Nothing

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
        \(headerText :: HeaderText) -> do
            checkMarkdown headerText isHeader headMaybe

    prop "should preserve header size" $
        \(headerText :: HeaderText) -> do
            checkMarkdown headerText
                (\headerSize -> isSameHeaderSize headerSize headerText)
                (\content -> do
                    blockContent          <- headMaybe content
                    (Header headerSize _) <- getHeader blockContent
                    return headerSize
                )
    prop "should preserve its content" $
        \(headerText :: HeaderText) -> do
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
        \(blockQuote :: BlockQuoteText) -> do
            checkMarkdown blockQuote isBlockQuote headMaybe

    prop "should preserve its content" $
        \(blockQuote :: BlockQuoteText) -> do
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
        \(codeBlock :: CodeBlockSection) -> do
            checkMarkdown codeBlock isCodeBlock headMaybe
    prop "should preserve its content" $
        \(codeBlock :: CodeBlockSection) -> do
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
newtype UnorderedList = UnorderedList
    { getUnorderedList :: [Text]
    } deriving Show

instance CommonMarkdown UnorderedList where
    render (UnorderedList list) = T.unlines $ map (\element -> "- " <> element) list

instance Arbitrary UnorderedList where
    arbitrary = UnorderedList <$> listOf1 genPrintableText

unorderedListSpec :: Spec
unorderedListSpec = describe "Unordered list" $ do
    prop "should parse unordered list as BulletList" $
        \(unorderedList :: UnorderedList) -> do
            checkMarkdown unorderedList isBulletList headMaybe

    prop "should preserve its content" $
        \(unorderedList :: UnorderedList) -> do
            checkMarkdown unorderedList
                (\renderedTexts -> renderedTexts == (getUnorderedList unorderedList))
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
newtype OrderedList = OrderedList
    { getOrderedList :: [Text]
    } deriving Show

instance Arbitrary OrderedList where
    arbitrary = OrderedList <$> listOf1 genPrintableText

instance CommonMarkdown OrderedList where
    render (OrderedList list) = T.unlines $
        zipWith (\num someText -> tshow num <> ". " <> someText) [1..] list

orderedListSpec :: Spec
orderedListSpec = describe "Ordered list" $ do
    prop "should parse ordered list as BulletList" $
        \(orderedList :: OrderedList) -> do
            checkMarkdown orderedList isBulletList headMaybe

    prop "should preserve its content" $
        \(orderedList :: OrderedList) -> do
            checkMarkdown orderedList
                (\renderedTexts -> renderedTexts == (getOrderedList orderedList))
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
imageSpec = do
    describe "Image" $ do
        prop "should parse image as Image" $
            \(imageSection :: ImageSection) -> do
                checkMarkdown imageSection isThumbnail headMaybe
        prop "should preserve its link" $
            \(imageSection :: ImageSection) -> do
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
        renderColumn contents' = foldl' (\acc a -> acc <> a <> " | ") "| " contents'

        renderBetween' :: Int -> Text
        renderBetween' rowNum' = T.replicate rowNum' "|- " <> "|"

        renderTableContent :: [[Text]] -> [Text]
        renderTableContent tables = map renderColumn tables

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
        \(table :: TableSection) -> do
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

----------------------------------------------------------------------------------------------------
-- Segments
----------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Link
--------------------------------------------------------------------------------

-- | Link segment
data LinkSegment = LinkSegment
    { linkName :: !Text
    , linkUrl  :: !Text
    } deriving Show

instance CommonMarkdown LinkSegment where
    render (LinkSegment name url) = "[" <> name <> "](" <> url <> ")"

instance Arbitrary LinkSegment where
    arbitrary = LinkSegment <$> genRandomText <*> genPrintableUrl

linkSpec :: Spec
linkSpec = describe "Links" $ do
    prop "should parse link as Link" $
        \(linkSegment :: LinkSegment) -> do
            checkMarkdown linkSegment isLink getHeadSegment

    prop "should preserve its content" $
        \(linkSegment :: LinkSegment) -> do
            checkMarkdown linkSegment
                (\(Link mName (Url url)) ->
                    and
                        [ url == linkUrl linkSegment
                        , maybe False (\name -> name == linkName linkSegment) mName
                        ]
                )
                (\content -> do
                    segment <- getHeadSegment content
                    getLink segment
                )
    prop "should not have any other segments except for code section" $
        \(linkSegment :: LinkSegment) -> testSegment linkSegment
  where
    getLink :: Segment -> Maybe Segment
    getLink linkSegment@(Link _ _) = Just linkSegment
    getLink _                      = Nothing

--------------------------------------------------------------------------------
-- CodeNotation
--------------------------------------------------------------------------------

-- | Code notation segment
newtype CodeNotationSegment = CodeNotationSegment
    { getCodeNotationSegment :: Text
    } deriving Show

instance CommonMarkdown CodeNotationSegment where
    render (CodeNotationSegment txt) = "`" <> txt <> "`"

instance Arbitrary CodeNotationSegment where
    arbitrary = CodeNotationSegment <$> genPrintableText

codeNotationSpec :: Spec
codeNotationSpec = do
    describe "Code notation" $ do
        prop "should be able to parser code section as CodeNotation" $
            \(codeNotation :: CodeNotationSegment) -> do
                checkMarkdown codeNotation isCodeNotation getHeadSegment

        prop "should preserve its content" $
            \(codeNotation :: CodeNotationSegment) -> do
                checkMarkdown codeNotation
                    (\codeText -> codeText == getCodeNotationSegment codeNotation)
                    (\content -> do
                        segment                 <- getHeadSegment content
                        (CodeNotation codeText) <- getCodeNotationText segment
                        return codeText
                    )
        prop "should not have any other segments except for code section" $
            \(codeNotation :: CodeNotationSegment) -> testSegment codeNotation
  where
    getCodeNotationText :: Segment -> Maybe Segment
    getCodeNotationText codeNotation@(CodeNotation _) = Just codeNotation
    getCodeNotationText _                             = Nothing

--------------------------------------------------------------------------------
-- SimpleText segment
--------------------------------------------------------------------------------

-- | Simple text segment
newtype SimpleTextSegment = SimpleTextSegment {
    getSimpleTextSegment :: Text
    } deriving Show

instance CommonMarkdown SimpleTextSegment where
    render (SimpleTextSegment txt) = txt

instance Arbitrary SimpleTextSegment where
    arbitrary = SimpleTextSegment <$> genPrintableText

plainTextSpec :: Spec
plainTextSpec = describe "Plain text" $ do
    prop "should parse plain text as SimpleText" $
        \(simpleTextSegment :: SimpleTextSegment) -> 
            checkMarkdown simpleTextSegment isSimpleText getHeadSegment

    prop "should preserve its content" $
        \(simpleTextSegment :: SimpleTextSegment) ->
            checkMarkdown simpleTextSegment
            (\(SimpleText txt) -> txt == getSimpleTextSegment simpleTextSegment)
            (\content -> do
                segment                 <- getHeadSegment content
                getSimpleText segment
            )

    prop "should not have any other segments except for plain text" $
        \(simpleTextSegment :: SimpleTextSegment) -> testSegment simpleTextSegment
  where
    getSimpleText :: Segment -> Maybe Segment
    getSimpleText simpleTextSegment@(SimpleText _) = Just simpleTextSegment
    getSimpleText _                                = Nothing

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

-- | Typeclass in which is used to render given datatype into common markdown format.
class CommonMarkdown a where
    render :: a -> Text

-- | Generate arbitrary Text
-- this is needed as some characters like
-- '`' and `>` will be parsed as blockquote, code notation, etc.
genPrintableText :: Gen Text
genPrintableText = T.unwords <$> listOf1 genRandomText

-- | Generate random text
genRandomText :: Gen Text
genRandomText = (fmap fromString) <$> listOf1 $ elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])

-- | Generate random url
genPrintableUrl :: Gen Text
genPrintableUrl = do
    end        <- elements [".org", ".edu", ".com", ".co.jp"]
    randomSite <- genRandomText
    return $ "http://www." <> randomSite <> end

-- | Parse given datatype into Markdown
parseMarkdown :: CommonMarkdown a => a -> Markdown
parseMarkdown = commonmarkToMarkdown optDefault . render

-- | General function used to test if given 'CommonMarkdown' can be properly parsed
-- and extract the expected element
checkMarkdown :: (CommonMarkdown a) 
              => a
              -> (parsedContent -> Bool)
              -> ([Block] -> Maybe parsedContent)
              -> Bool
checkMarkdown markdown pre extractionFunc = do
    let (Markdown content) = parseMarkdown markdown
    maybe False pre (extractionFunc content)

-- | Extract heed segment of a given list of blocks
getHeadSegment :: [Block] -> Maybe Segment
getHeadSegment blocks = do
    blockContent                 <- headMaybe blocks
    (Paragraph (ScrapText ctxs)) <- getParagraph blockContent
    (Context _ segments)         <- headMaybe ctxs
    segment                      <- headMaybe segments
    return segment

-- | General test case to check whether the segment was parsed properly
testSegment :: (CommonMarkdown section) => section -> Bool
testSegment someSegment = do
    checkMarkdown someSegment
        (\(content', ctxs, segments) ->
            and [ length content' == 1
                , length ctxs     == 1
                , length segments == 1
                ]
        )
        (\content -> do
            blockContent                 <- headMaybe content
            (Paragraph (ScrapText ctxs)) <- getParagraph blockContent
            (Context _ segments)         <- headMaybe ctxs
            return (content, ctxs, segments)
        )
