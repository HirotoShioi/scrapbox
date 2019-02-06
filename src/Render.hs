{-| Render module, these are used to render given 'Scrapbox' into 'Text' using
'renderPretty' or 'renderRaw'

You can also use 'writeScrapbox' to write given 'Scrapbox' into file.
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Render
    ( -- * Exposed interface
      renderPretty
    , renderRaw
    , writeScrapbox
    -- * Exposed for testing
    , renderBlock
    , renderContent
    , renderText
    ) where

import           RIO
import qualified RIO.Text as T

import           Types    (Block (..), CodeName (..), CodeSnippet (..), Content,
                           Context (..), Level (..), ScrapText (..),
                           Scrapbox (..), Segment (..), Start (..), Style (..),
                           StyleData (..), TableContent (..), TableName (..),
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
    CODE_BLOCK codeName code     -> renderCodeBlock codeName code <> renderBlock LINEBREAK
    PARAGRAPH stext              -> [renderText stext]
    HEADING level contents       -> [renderHeading level contents]
    TABLE tableName tableContent -> renderTable tableName tableContent <> renderBlock LINEBREAK
    THUMBNAIL (Url url)          -> [blocked url]

-- | Render given 'ScrapText' into 'Text'
renderText :: ScrapText -> Text
renderText (ScrapText ctxs) = foldr (\scrap acc-> renderScrapText scrap <> acc) mempty ctxs

-- | Render given 'Context' into 'Text'
renderScrapText :: Context -> Text
renderScrapText (Context style content) = renderWithStyle style content

-- | Render given 'Content' to 'Text'
renderContent :: Content -> Text
renderContent = foldr (\ctx acc -> renderSegment ctx <> acc) mempty
  where
    renderSegment :: Segment -> Text
    renderSegment = \case
        CODE_NOTATION code             -> "`" <> code <> "`"
        HASHTAG text                   -> "#" <> text
        LINK (Just linkName) (Url url) -> blocked $ url <> " " <> linkName
        LINK Nothing (Url url)         -> blocked url
        TEXT text                      -> text

-- | Render 'CODE_BLOCK'
renderCodeBlock :: CodeName -> CodeSnippet -> [Text]
renderCodeBlock (CodeName name) (CodeSnippet code) = do
    let codeName = "code:" <> name
    let codeContent = map (" " <>) (T.lines code)
    [codeName] <> codeContent

-- | Render 'TABLE'
renderTable :: TableName -> TableContent -> [Text]
renderTable (TableName name) (TableContent content) =
    let title = ["table:" <> name]
        renderdTable = map (foldr (\someText acc -> "\t" <> someText <> acc) mempty) content
    in title <> renderdTable

-- | Render 'BULLET_POINT'
renderBulletPoint :: Start -> [Block] -> [Text]
renderBulletPoint (Start num) = concatMap (map (\text -> T.replicate num "\t" <> text) . renderBlock)

-- | Add an block to a given renderd text
blocked :: Text -> Text
blocked content = "[" <> content <> "]"

-- | Render with style
renderWithStyle :: Style -> Content -> Text
renderWithStyle style ctx = case style of
    NoStyle -> renderContent ctx
    Bold ->
        let boldStyle = StyleData 0 True False False
        in renderCustomStyle boldStyle ctx
    Italic ->
        let italicStyle = StyleData 0 False True False
        in renderCustomStyle italicStyle ctx
    StrikeThrough ->
        let strikeThroughStyle = StyleData 0 False False True
        in renderCustomStyle strikeThroughStyle ctx
    CustomStyle customStyle -> renderCustomStyle customStyle ctx
    UserStyle userStyle -> "[" <> userStyle <> " " <> renderContent ctx <> "]"

renderCustomStyle :: StyleData -> Content -> Text
renderCustomStyle (StyleData headerNum isBold isItalic isStrikeThrough) content =
    let italicSymbol         = if isItalic then "/" else mempty
        strikeThroughSymbol  = if isStrikeThrough then "-" else mempty
        boldSymbol           = if isBold then "*" else mempty
        headerNum'           = if isBold then 0 else headerNum
        combinedSyntax       = mconcat
            [ T.replicate headerNum' "*"
            , boldSymbol
            , italicSymbol
            , strikeThroughSymbol
            , " "
            ]
    in blocked $ combinedSyntax <> renderContent content

-- | Render 'HEADING' block
renderHeading :: Level -> Content -> Text
renderHeading (Level headerSize) content =
    let style = StyleData headerSize False False False
    in renderCustomStyle style content
