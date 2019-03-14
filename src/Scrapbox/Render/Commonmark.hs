{-| Render commonmark
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrapbox.Render.Commonmark
    ( renderToCommonmark
    ) where

import           RIO
import qualified RIO.Text       as T
import           Scrapbox.Types


renderToCommonmark :: Scrapbox -> Text
renderToCommonmark (Scrapbox blocks) = T.unlines $ concatMap renderBlock blocks

renderBlock :: Block -> [Text]
renderBlock = \case
    LINEBREAK                       -> ["\\"]
    BLOCK_QUOTE scraptext           -> [">" <> renderScrapText scraptext]
    BULLET_POINT start blocks       -> renderBulletPoint start blocks -- (map renderBlock blocks)
    CODE_BLOCK codeName codeSnippet -> renderCodeblock codeName codeSnippet
    HEADING level segments          -> [renderHeading level segments]
    PARAGRAPH scraptext             -> [renderScrapText scraptext]
    TABLE tableName tableContent    -> [""]
    THUMBNAIL (Url url)             -> ["![image](" <> url <> ")"]

renderScrapText :: ScrapText -> Text
renderScrapText (ScrapText inlineBlocks) =
    -- Add space
    T.unwords $ map renderInlineBlock inlineBlocks

renderBulletPoint :: Start -> [Block] -> [Text]
renderBulletPoint (Start startnum) blocks = [""]

renderHeading :: Level -> [Segment] -> Text
renderHeading (Level headingNum) segments =
    let level = case headingNum of
                4 -> 1
                3 -> 2
                2 -> 3
                1 -> 4
                _ -> 4
        renderedSegments = foldr ( (<>) . renderSegment) mempty segments
        renderedLevel    = T.replicate level "#"
    in  renderedLevel <> " " <> renderedSegments

renderSegment ::  Segment -> Text
renderSegment = \case
    HASHTAG text               -> "**#" <> text <> "**"
    LINK Nothing (Url url)     -> url
    LINK (Just name) (Url url) -> "[" <> name <> "]" <> "(" <> url <> ")"
    TEXT text                  -> text

renderInlineBlock :: InlineBlock -> Text
renderInlineBlock (CODE_NOTATION text)   = "`" <> text <> "`"
renderInlineBlock (MATH_EXPRESSION text) = "`" <> text <> "`" -- Does commonmark support math expressions?
renderInlineBlock (ITEM style segments)  =
    let renderedSegments = foldr ( (<>) . renderSegment) mempty segments
    in case style of
        Bold          -> "**" <> renderedSegments <> "**"
        Italic        -> "_" <> renderedSegments <> "_"
        NoStyle       -> renderedSegments
        StrikeThrough -> "~~" <> renderedSegments <> "~~"

renderCodeblock :: CodeName -> CodeSnippet -> [Text]
renderCodeblock (CodeName name) (CodeSnippet snippet) =
    [name] <> ["```"] <> snippet <> ["```"]
