{-| Render module, these are used to render given 'Scrapbox' into 'Text' using
'renderPretty' or 'renderRaw'

You can also use 'writeScrapbox' to write given 'Scrapbox' into file.
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrapbox.Render
    ( renderPretty
    , renderRaw
    , writeScrapbox
    , renderBlock
    , renderSegments
    , renderText
    , renderInline
    ) where

import           RIO
import qualified RIO.Text       as T

import           Scrapbox.Types (Block (..), CodeName (..), CodeSnippet (..),
                                 InlineBlock (..), Level (..), ScrapText (..),
                                 Scrapbox (..), Segment (..), Start (..),
                                 Style (..), TableContent (..), TableName (..),
                                 Url (..))

--------------------------------------------------------------------------------
-- Exposed interface
--------------------------------------------------------------------------------

-- | Pretty print 'Scrapbox'
renderPretty :: Scrapbox -> Text
renderPretty (Scrapbox blocks) = T.unlines $ concatMap renderBlock blocks

-- | Render given 'Scrapbox' into list of 'ByteString'
renderRaw :: Scrapbox -> [ByteString]
renderRaw (Scrapbox blocks) =
    concatMap (map encodeUtf8 . renderBlock) blocks

-- | Write given 'Scrapbox' to given path
writeScrapbox :: FilePath -> Scrapbox -> IO ()
writeScrapbox path (Scrapbox blocks) = do
    let renderedScrapbox = T.unlines $ concatMap renderBlock blocks
    writeFileUtf8 path renderedScrapbox

--------------------------------------------------------------------------------
-- Rendering logics
--------------------------------------------------------------------------------

-- | Render given 'Block' into  'Text'
renderBlock :: Block -> [Text]
renderBlock = \case
    LINEBREAK                    -> [""]
    BLOCK_QUOTE stext            -> [">" <> renderText stext]
    BULLET_POINT start blocks    -> renderBulletPoint start blocks
    CODE_BLOCK codeName code     -> renderCodeBlock codeName code
    PARAGRAPH stext              -> [renderText stext]
    HEADING level contents       -> [renderHeading level contents]
    TABLE tableName tableContent -> renderTable tableName tableContent
    THUMBNAIL (Url url)          -> [blocked url]

-- | Render given 'ScrapText' into 'Text'
renderText :: ScrapText -> Text
renderText (ScrapText inlines) =
    foldr (\scrap acc-> renderInline scrap <> acc) mempty inlines

-- | Render given 'InlineBlock' into 'Text'
renderInline :: InlineBlock -> Text
renderInline (ITEM style content)    = renderWithStyle style content
renderInline (CODE_NOTATION content) = "`" <> content <> "`"
renderInline (MATH_EXPRESSION expr)  = "[$ " <> expr <> "]"

-- | Render given 'Content' to 'Text'
renderSegments :: [Segment] -> Text
renderSegments = foldr (\inline acc -> renderSegment inline <> acc) mempty
  where
    renderSegment :: Segment -> Text
    renderSegment = \case
        HASHTAG text                   -> "#" <> text
        LINK (Just linkName) (Url url) -> blocked $ url <> " " <> linkName
        LINK Nothing (Url url)         -> blocked url
        TEXT text                      -> text

-- | Render 'CODE_BLOCK'
renderCodeBlock :: CodeName -> CodeSnippet -> [Text]
renderCodeBlock (CodeName name) (CodeSnippet code) = do
    let codeName = "code:" <> name
    let codeContent = map (" " <>) code
    [codeName] <> codeContent

-- | Render 'TABLE'
renderTable :: TableName -> TableContent -> [Text]
renderTable (TableName name) (TableContent [[]])    = ["table:" <> name]
renderTable (TableName name) (TableContent content) =
    let title = ["table:" <> name]
        renderedTable = map
                 (foldr (\someText acc -> "\t" <> someText <> acc) mempty)
                 content
    in title <> renderedTable

-- | Render 'BULLET_POINT'
renderBulletPoint :: Start -> [Block] -> [Text]
renderBulletPoint (Start num) =
    concatMap (map (\text -> T.replicate num "\t" <> text) . renderBlock)

-- | Add an block to a given renderd text
blocked :: Text -> Text
blocked content = "[" <> content <> "]"

-- | Render with style
renderWithStyle :: [Style] -> [Segment] -> Text
renderWithStyle styles inline = case styles of
    []     -> renderSegments inline
    [Bold] -> "[[" <> renderSegments inline <> "]]"
    others ->
        let syntax = mkStyleSyntax others
            renderedSegments = renderSegments inline
        in "[" <> syntax <> " " <> renderedSegments <> "]"

mkStyleSyntax :: [Style] -> Text
mkStyleSyntax = go mempty -- Need to sort
  where
    go :: Text -> [Style] -> Text
    go styleSyntax []                = styleSyntax
    go syntax  (Bold:xs)             = go ("*" <> syntax) xs
    go syntax (Italic:xs)            = go ("/" <> syntax) xs
    go syntax (StrikeThrough:xs)     = go ("-" <> syntax) xs
    go syntax (Sized (Level num):xs) = go (T.replicate num "*" <> syntax) xs
    go syntax (UserStyle style:xs)   = go (style <> syntax) xs

-- | Render 'HEADING' block
renderHeading :: Level -> [Segment] -> Text
renderHeading (Level headerSize) content =
    let syntax = T.replicate headerSize "*"
        renderedContent = renderSegments content
    in "[" <> syntax <> " " <> renderedContent <> "]"
