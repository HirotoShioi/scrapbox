{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           RIO
import           RIO.List              (headMaybe)

import           Test.Hspec            (Spec, describe, hspec)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, elements, listOf1)

import           CommonMark.Lib        (commonmarkToMarkdown, optDefault)
import           Render                (renderContent, renderText)
import           Types                 (Block (..), HeaderSize (..),
                                        Markdown (..), isBlockQuote, isHeader)

main :: IO ()
main = hspec $ do
    commonMarkSpec

commonMarkSpec :: Spec
commonMarkSpec = describe "Common mark" $ do
    headerTextSpec
    blockQuoteSpec

-- | Get 'Header'
getHeader :: Block -> Maybe Block
getHeader header@(Header _ _) = Just header
getHeader _                   = Nothing

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Test spec for Header text
headerTextSpec :: Spec
headerTextSpec = describe "Header text" $ modifyMaxSuccess (const 1000) $ do
    prop "should be able to parse header text as header" $
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

-- | Data type for common mark Header
data HeaderText
    = H1 Text
    | H2 Text
    | H3 Text
    | H4 Text
    | H5 Text
    | H6 Text
    deriving Show

-- | Check if given headerSize is same size
isSameHeaderSize :: HeaderSize -> HeaderText -> Bool
isSameHeaderSize (HeaderSize 4) (H1 _) = True
isSameHeaderSize (HeaderSize 3) (H2 _) = True
isSameHeaderSize (HeaderSize 2) (H3 _) = True
isSameHeaderSize (HeaderSize 1) (H4 _) = True
isSameHeaderSize (HeaderSize 1) (H5 _) = True
isSameHeaderSize (HeaderSize 1) (H6 _) = True
isSameHeaderSize _ _                   = False

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
blockQuoteSpec = describe "BlockQuote text" $ modifyMaxSuccess (const 1000) $ do
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

newtype BlockQuoteText = BlockQuoteText
    { getBlockQuoteText :: Text
    } deriving Show

instance CommonMarkdown BlockQuoteText where
    render (BlockQuoteText txt) = ">" <> txt

instance Arbitrary BlockQuoteText where
    arbitrary = BlockQuoteText <$> genPrintableText

-- Should this function be here?
getBlockQuote :: Block -> Maybe Block
getBlockQuote blockQuote@(BlockQuote _) = Just blockQuote
getBlockQuote _                         = Nothing

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
genPrintableText = fromString <$> genRandomString
  where
    genRandomString :: Gen String
    genRandomString = listOf1 $ elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])

-- | Parse given datatype into Markdown
parseMarkdown :: CommonMarkdown a => a -> Markdown
parseMarkdown = commonmarkToMarkdown optDefault . render

-- Maybe not Bool but something else?
checkMaybe :: (a -> Bool) -> Maybe a -> Bool
checkMaybe pre mSomething = maybe False pre mSomething
