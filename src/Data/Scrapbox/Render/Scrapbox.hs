{-| Render module, defining the function which renders given 'Scrapbox' into 'Text'
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Render.Scrapbox
    ( renderToScrapboxNoOption
    , renderBlock
    , renderSegments
    , renderScrapText
    , renderInline
    , renderWithStyle
    ) where

import           RIO
import           RIO.List (nub)
import qualified RIO.Text as T

import           Data.Scrapbox.Types (Block (..), CodeName (..),
                                      CodeSnippet (..), InlineBlock (..),
                                      Level (..), ScrapText (..), Scrapbox (..),
                                      Segment (..), Start (..), Style (..),
                                      TableContent (..), TableName (..),
                                      Url (..))

--------------------------------------------------------------------------------
-- Exposed interface
--------------------------------------------------------------------------------

-- | Render given 'Scrapbox' AST into Scrapbox page
renderToScrapboxNoOption :: Scrapbox -> Text
renderToScrapboxNoOption (Scrapbox blocks) = T.unlines $ concatMap renderBlock blocks

--------------------------------------------------------------------------------
-- Rendering logics
--------------------------------------------------------------------------------

-- | Render given 'Block' into  'Text'
renderBlock :: Block -> [Text]
renderBlock = \case
    LINEBREAK                    -> [""]
    BLOCK_QUOTE stext            -> [">" <> renderScrapText stext]
    BULLET_POINT start blocks    -> renderBulletPoint start blocks
    CODE_BLOCK codeName code     -> renderCodeBlock codeName code
    PARAGRAPH stext              -> [renderScrapText stext]
    HEADING level contents       -> [renderHeading level contents]
    TABLE tableName tableContent -> renderTable tableName tableContent
    THUMBNAIL (Url url)          -> [blocked url]

-- | Render given 'ScrapText' into 'Text'
renderScrapText :: ScrapText -> Text
renderScrapText (ScrapText inlines) =
    foldr (\scrap acc-> renderInline scrap <> acc) mempty inlines

-- | Render given 'InlineBlock' into 'Text'
renderInline :: InlineBlock -> Text
renderInline (SPAN style content)    = renderWithStyle style content
renderInline (CODE_NOTATION content) = "`" <> content <> "`"
renderInline (MATH_EXPRESSION expr)  = "[$ " <> expr <> "]"

-- | Render given @[Segment]@ to 'Text'
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
renderWithStyle :: [Style] -> [Segment] -> Text
renderWithStyle styles inline = case styles of
    []     -> renderSegments inline
    [Bold] -> "[[" <> renderSegments inline <> "]]"
    others ->
        let syntax = mkStyleSyntax (nub others)
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
