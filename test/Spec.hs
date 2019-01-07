{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           RIO
import           RIO.List              (headMaybe, zipWith)
import qualified RIO.Text              as T

import           Test.Hspec            (Spec, describe, hspec)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, elements, listOf1)

import           CommonMark.Lib        (commonmarkToMarkdown, optDefault)
import           Render                (renderContent, renderText, renderBlock)
import           Types                 (Block (..), CodeSnippet (..),
                                        Context (..), HeaderSize (..),
                                        Markdown (..), ScrapText (..),
                                        Segment (..), isBlockQuote, isCodeBlock,
                                        isCodeNotation, isHeader, isParagraph, isBulletList)

main :: IO ()
main = hspec $ do
    commonMarkSpec

commonMarkSpec :: Spec
commonMarkSpec = describe "CommonMark parser" $ modifyMaxSuccess (const 200) $ do
    paragraphSpec
    codeNotationSpec
    headerTextSpec
    blockQuoteSpec
    codeBlockSpec
    unorderedListSpec
    orderedListSpec

--------------------------------------------------------------------------------
-- Paragraph
--------------------------------------------------------------------------------

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
            let (Markdown content) = parseMarkdown paragraph
            checkMaybe
                (\blockContent -> isParagraph blockContent)
                (headMaybe content)

    prop "should preserve its content" $
        \(paragraph :: ParagraphSection) -> do
            let (Markdown content) = parseMarkdown paragraph
            checkMaybe
                (\paragraphText -> paragraphText == (getParagraphSection paragraph))
                (do
                    blockContent 　　　<- headMaybe content
                    (Paragraph stext) <- getParagraph blockContent
                    return $ renderText stext
                )

getParagraph :: Block -> Maybe Block
getParagraph paragraph@(Paragraph _) = Just paragraph
getParagraph _                       = Nothing

--------------------------------------------------------------------------------
-- CodeNotation
--------------------------------------------------------------------------------

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
                let (Markdown content) = parseMarkdown codeNotation
                checkMaybe
                    (\segment -> isCodeNotation segment)
                    (getHeadSegment content)

        prop "should preserve its content" $
            \(codeNotation :: CodeNotationSegment) -> do
                let (Markdown content) = parseMarkdown codeNotation
                checkMaybe
                    (\codeText -> codeText == getCodeNotationSegment codeNotation)
                    (do
                        segment                 <- getHeadSegment content
                        (CodeNotation codeText) <- getCodeNotationText segment
                        return codeText
                    )
        prop "should not have any other segments except for code section" $
            \(codeNotation :: CodeNotationSegment) -> do
                let (Markdown content) = parseMarkdown codeNotation
                checkMaybe
                    (\(content', ctxs, segments) -> 
                        and [ length content' == 1
                            , length ctxs == 1
                            , length segments == 1
                            ]
                    )
                    (do
                        blockContent                 <- headMaybe content
                        (Paragraph (ScrapText ctxs)) <- getParagraph blockContent
                        (Context _ segments)         <- headMaybe ctxs   
                        return (content, ctxs, segments)       
                    )
  where
    getCodeNotationText :: Segment -> Maybe Segment
    getCodeNotationText codeNotation@(CodeNotation _) = Just codeNotation
    getCodeNotationText _                             = Nothing

getHeadSegment :: [Block] -> Maybe Segment
getHeadSegment blocks = do
    blockContent                 <- headMaybe blocks
    (Paragraph (ScrapText ctxs)) <- getParagraph blockContent
    (Context _ segments)         <- headMaybe ctxs
    segment                      <- headMaybe segments
    return segment

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Test spec for Header text
headerTextSpec :: Spec
headerTextSpec = describe "Header text" $ do
    prop "should be able to parse header text as Header" $
        \(headerText :: HeaderText) -> do
            let (Markdown content) = parseMarkdown headerText
            checkMaybe
                (\blockContent -> isHeader blockContent)
                (headMaybe content)

    prop "should preserve header size" $
        \(headerText :: HeaderText) -> do
            let (Markdown content) = parseMarkdown headerText
            checkMaybe
                (\headerSize -> isSameHeaderSize headerSize headerText)
                (do
                    blockContent          <- headMaybe content
                    (Header headerSize _) <- getHeader blockContent
                    return headerSize
                )
    prop "should preserve its content" $
        \(headerText :: HeaderText) -> do
            let (Markdown content) = parseMarkdown headerText
            checkMaybe
                (\headerContent -> headerContent == getHeaderTextContent headerText)
                (do
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

-- | Data type for common mark Header
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

--------------------------------------------------------------------------------
-- BlockQuote
--------------------------------------------------------------------------------

blockQuoteSpec :: Spec
blockQuoteSpec = describe "BlockQuote text" $ do
    prop "should be able parse block quote text as BlockQuote" $
        \(blockQuote :: BlockQuoteText) -> do
            let (Markdown content) = parseMarkdown blockQuote
            checkMaybe
                (\blockContent -> isBlockQuote blockContent)
                (headMaybe content)

    prop "should preserve its content" $
        \(blockQuote :: BlockQuoteText) -> do
            let (Markdown content) = parseMarkdown blockQuote
            checkMaybe
                (\quoteText -> quoteText == getBlockQuoteText blockQuote)
                (do
                    blockContent       <- headMaybe content
                    (BlockQuote stext) <- getBlockQuote blockContent
                    return $ renderText stext
                    )
  where
    -- Should this function be here?
    getBlockQuote :: Block -> Maybe Block
    getBlockQuote blockQuote@(BlockQuote _) = Just blockQuote
    getBlockQuote _                         = Nothing


newtype BlockQuoteText = BlockQuoteText
    { getBlockQuoteText :: Text
    } deriving Show

instance CommonMarkdown BlockQuoteText where
    render (BlockQuoteText txt) = ">" <> txt

instance Arbitrary BlockQuoteText where
    arbitrary = BlockQuoteText <$> genPrintableText

--------------------------------------------------------------------------------
-- CodeBlock
--------------------------------------------------------------------------------

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
            let (Markdown content) = parseMarkdown codeBlock
            checkMaybe
                (\blockContent -> isCodeBlock blockContent)
                (headMaybe content)
    prop "should preserve its content" $
        \(codeBlock :: CodeBlockSection) -> do
            let (Markdown content) = parseMarkdown codeBlock
            checkMaybe
                (\codeContent -> codeContent == T.unlines (getCodeBlockContent codeBlock))
                (do
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
            let (Markdown content) = parseMarkdown unorderedList
            checkMaybe
                (\blockContent -> isBulletList blockContent)
                (headMaybe content)

    prop "should preserve its content" $
        \(unorderedList :: UnorderedList) -> do
            let (Markdown content) = parseMarkdown unorderedList
            checkMaybe
                (\renderedTexts -> renderedTexts == (getUnorderedList unorderedList))
                (do
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
            let (Markdown content) = parseMarkdown orderedList
            checkMaybe
                (\blockContent -> isBulletList blockContent)
                (headMaybe content)
        
    prop "should preserve its content" $
        \(orderedList :: OrderedList) -> do
            let (Markdown content) = parseMarkdown orderedList
            checkMaybe
                (\renderedTexts -> renderedTexts == (getOrderedList orderedList))
                (do
                    blockContent       <- headMaybe content
                    (BulletList lists) <- getBulletList blockContent
                    return $ concatMap renderBlock lists
                )

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

-- | Typeclass in which is used to render given datatype into common markdown format.
class CommonMarkdown a where
    render     :: a -> Text

-- | Generate arbitrary Text
-- this is needed as some characters like
-- '`' and `>` will be parsed as blockquote, code notation, etc.
genPrintableText :: Gen Text
genPrintableText = (fromString . unwords) <$> listOf1 genRandomString
  where
    genRandomString :: Gen String
    genRandomString = listOf1 $ elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])

-- | Parse given datatype into Markdown
parseMarkdown :: CommonMarkdown a => a -> Markdown
parseMarkdown = commonmarkToMarkdown optDefault . render

-- Maybe not Bool but something else?
checkMaybe :: (a -> Bool) -> Maybe a -> Bool
checkMaybe pre mSomething = maybe False pre mSomething
