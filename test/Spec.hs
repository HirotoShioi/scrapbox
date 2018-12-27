{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           RIO
import           RIO.List

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           CommonMark.Lib
import           Types
import           Render

main :: IO ()
main = hspec $ do
    commonMarkSpec

commonMarkSpec :: Spec
commonMarkSpec = describe "Common mark" $ do
    headerTextSpec

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

headerTextSpec :: Spec
headerTextSpec = describe "Header text" $ modifyMaxSuccess (const 1000) $ do
    prop "should be able to render header text as header" $
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
                    blockContent <- headMaybe content
                    headerSize   <- getHeaderSize blockContent
                    return headerSize
                )
    prop "should preserve its content" $
        \(headerText :: HeaderText) -> do
            let (Markdown content) = parseMarkdown headerText
            checkMaybe
                (\headerContent -> headerContent == getHeaderTextContent headerText)
                (do
                    blockContent  <- headMaybe content
                    headerContent <- getHeaderContent blockContent
                    return $ renderContent headerContent
                )
  where
    checkMaybe :: (a -> Bool) -> Maybe a -> Bool
    checkMaybe pre mSomething = maybe False (\something -> pre something) mSomething 

-- | Data type for common mark Header
data HeaderText
    = H1 Text
    | H2 Text
    | H3 Text
    | H4 Text
    deriving Show

-- | Check if given headerSize is same size
isSameHeaderSize :: HeaderSize -> HeaderText -> Bool
isSameHeaderSize (HeaderSize 4) (H1 _) = True
isSameHeaderSize (HeaderSize 3) (H2 _) = True
isSameHeaderSize (HeaderSize 2) (H3 _) = True
isSameHeaderSize (HeaderSize 1) (H4 _) = True
isSameHeaderSize _ _                   = False

-- | Get the content of the 'HeaderText'
getHeaderTextContent :: HeaderText -> Text
getHeaderTextContent = \case
    H1 txt -> txt
    H2 txt -> txt
    H3 txt -> txt
    H4 txt -> txt

instance Arbitrary HeaderText where
    arbitrary = join $ elements
        [ H1 <$> genPrintableText
        , H2 <$> genPrintableText
        , H3 <$> genPrintableText
        , H4 <$> genPrintableText
        ]

instance CommonMarkdown HeaderText where
    render = \case
        H1 textContent -> "# " <> textContent
        H2 textContent -> "## " <> textContent
        H3 textContent -> "### " <> textContent
        H4 textContent -> "#### " <> textContent

-- | Parse given datatype into Markdown
parseMarkdown :: CommonMarkdown a => a -> Markdown
parseMarkdown = commonmarkToMarkdown optDefault . render

-- | Typeclass in which is used to render given datatype into common markdown format.
class CommonMarkdown a where
    render     :: a -> Text

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

-- | Generate arbitrary Text
-- this is needed as some characters like '`' and `>` will be parsed as blockquote, code notation, etc.
genPrintableText :: Gen Text
genPrintableText = fromString <$> genRandomString
  where
    genRandomString :: Gen String
    genRandomString = listOf1 $ elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])