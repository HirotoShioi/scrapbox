{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Render
    ( -- * Exposed interface
      renderPretty
    , renderRaw
    , writeMarkdown
    -- * For testing
    , renderBlock
    , renderContent
    , renderText
    ) where

import           RIO
import qualified RIO.Text as T

import           Types    (Block (..), BulletSize (..), CodeName (..),
                           CodeSnippet (..), Content, Context (..),
                           HeaderSize (..), Markdown (..), ScrapText (..),
                           Segment (..), Style (..), StyleData (..),
                           TableContent (..), TableName (..), Url (..))

--------------------------------------------------------------------------------
-- Exposed interface
--------------------------------------------------------------------------------

-- | Pretty print Markdown
renderPretty :: Markdown -> Text
renderPretty (Markdown blocks) = T.unlines $ concatMap renderBlock blocks

-- | render given `Markdown' into list of ByteStrings
renderRaw :: Markdown -> [ByteString]
renderRaw (Markdown blocks) =
    concatMap (map encodeUtf8 . renderBlock) blocks

-- | Write markdown to given path
writeMarkdown :: FilePath -> Markdown -> IO ()
writeMarkdown path (Markdown blocks) = do
    let renderedMarkdown = T.unlines $ concatMap renderBlock blocks
    writeFileUtf8 path renderedMarkdown

--------------------------------------------------------------------------------
-- Rendering logics
--------------------------------------------------------------------------------

-- Render blocks
renderBlock :: Block -> [Text]
renderBlock = \case
    LineBreak                          -> [""]
    BlockQuote stext                   -> [">" <> renderText stext]
    BulletList contents                -> renderBulletPoints contents
    BulletPoint (BulletSize num) stext -> [T.replicate num " " <> renderText stext]
    CodeBlock codeName code            -> renderCodeBlock codeName code <> renderBlock LineBreak
    Paragraph stext                    -> [renderText stext]
    Header num contents                -> [renderHeader num contents]
    Table tableName tableContent       -> renderTable tableName tableContent <> renderBlock LineBreak
    Thumbnail (Url url)                -> [blocked url]

-- | render given 'ScrapText' into 'Text'
renderText :: ScrapText -> Text
renderText (ScrapText ctxs) = foldr (\scrap acc-> renderScrapText scrap <> acc) mempty ctxs

-- | render given 'Context' into 'Text'
renderScrapText :: Context -> Text
renderScrapText (Context style content) = renderWithStyle style content

-- | render given 'Content' to 'Text'
renderContent :: Content -> Text
renderContent = foldr (\ctx acc -> renderSegment ctx <> acc) mempty
  where
    renderSegment :: Segment -> Text
    renderSegment = \case
        CodeNotation code              -> "`" <> code <> "`"
        HashTag text                   -> "#" <> text
        Link (Just linkName) (Url url) -> blocked $ url <> " " <> linkName
        Link Nothing (Url url)         -> blocked url
        SimpleText text                -> text

-- | Render 'CodeBlock'
renderCodeBlock :: CodeName -> CodeSnippet -> [Text]
renderCodeBlock (CodeName name) (CodeSnippet code) = do
    let codeName = "code:" <> name
    let codeContent = map (\line -> " " <> line) (T.lines code)
    [codeName] <> codeContent

-- | Render 'Table'
renderTable :: TableName -> TableContent -> [Text]
renderTable (TableName name) (TableContent content) =
    let title = ["table:" <> name]
        renderdTable = map (foldr (\someText acc -> "\t" <> someText <> acc) mempty) content
    in title <> renderdTable

-- | render bulletpoints
renderBulletPoints :: [Block] -> [Text]
renderBulletPoints = concatMap (map (\ text -> "\t" <> text) . renderBlock)

-- | Add an block to a given renderd text
blocked :: Text -> Text
blocked content = "[" <> content <> "]"

-- | render with style (Do not export this)
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

-- | Render header
renderHeader :: HeaderSize -> Content -> Text
renderHeader (HeaderSize headerSize) content =
    let style = StyleData headerSize False False False
    in renderCustomStyle style content
