{-| Render module, these are used to render given 'Scrapbox' into 'Text' using
'renderToScrapbox' or 'renderRaw'

You can also use 'writeScrapbox' to write given 'Scrapbox' into file.
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Render.Scrapbox
    ( renderToS
    , renderBlock
    , renderSegments
    , renderText
    , renderInline
    ) where

import           RIO
import qualified RIO.Text as T

import           Data.Scrapbox.Types (Block (..), CodeName (..),
                                      CodeSnippet (..), InlineBlock (..),
                                      Level (..), ScrapText (..), Scrapbox (..),
                                      Segment (..), Start (..), Style (..),
                                      StyleData (..), TableContent (..),
                                      TableName (..), Url (..))

--------------------------------------------------------------------------------
-- Exposed interface
--------------------------------------------------------------------------------

-- | Render given 'Scrapbox' AST into Scrapbox page
renderToS :: Scrapbox -> Text
renderToS (Scrapbox blocks) = T.unlines $ concatMap renderBlock blocks

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
    let codeName    = "code:" <> name
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
renderWithStyle :: Style -> [Segment] -> Text
renderWithStyle style inline = case style of
    NoStyle -> renderSegments inline
    Bold   -> "[[" <> renderSegments inline <> "]]"
    Italic ->
        let italicStyle = StyleData 0 False True False
        in renderCustomStyle italicStyle inline
    StrikeThrough ->
        let strikeThroughStyle = StyleData 0 False False True
        in renderCustomStyle strikeThroughStyle inline
    CustomStyle customStyle -> renderCustomStyle customStyle inline
    UserStyle userStyle -> "[" <> userStyle <> " " <> renderSegments inline <> "]"

renderCustomStyle :: StyleData -> [Segment] -> Text
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
    in blocked $ combinedSyntax <> renderSegments content

-- | Render 'HEADING' block
renderHeading :: Level -> [Segment] -> Text
renderHeading (Level headerSize) content =
    let style = StyleData headerSize False False False
    in renderCustomStyle style content
