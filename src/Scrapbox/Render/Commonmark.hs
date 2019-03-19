{-| Render commonmark
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrapbox.Render.Commonmark
    ( renderToCommonmark
    ) where

import           RIO
import           RIO.List       (foldl', headMaybe, tailMaybe)
import qualified RIO.Text       as T
import           Scrapbox.Types (Block (..), CodeName (..), CodeSnippet (..),
                                 InlineBlock (..), Level (..), ScrapText (..),
                                 Scrapbox (..), Segment (..), Start (..),
                                 Style (..), StyleData (..), TableContent (..),
                                 TableName (..), Url (..))

-- | Render given 'Scrapbox' AST into commonmark
renderToCommonmark :: Scrapbox -> Text
renderToCommonmark (Scrapbox blocks) = T.unlines
    $ concatMap renderBlock
    $ addLineBreaks blocks
  where
    addLineBreaks :: [Block] -> [Block]
    addLineBreaks []               = []
    addLineBreaks [x]              = [x]
    addLineBreaks (LINEBREAK:xs)   = LINEBREAK : addLineBreaks xs
    addLineBreaks (x:LINEBREAK:xs) = x : LINEBREAK : addLineBreaks xs
    addLineBreaks (x:xs)           = x : LINEBREAK : addLineBreaks xs

renderBlock :: Block -> [Text]
renderBlock = \case
    LINEBREAK                       -> [""]
    BLOCK_QUOTE scraptext           -> [">" <> renderScrapText scraptext]
    BULLET_POINT start blocks       -> renderBulletPoint start blocks
    CODE_BLOCK codeName codeSnippet -> renderCodeblock codeName codeSnippet
    HEADING level segments          -> [renderHeading level segments]
    PARAGRAPH scraptext             -> [renderScrapText scraptext]
    TABLE tableName tableContent    -> renderTable tableName tableContent
    THUMBNAIL (Url url)             -> ["![image](" <> url <> ")"]

renderScrapText :: ScrapText -> Text
renderScrapText (ScrapText inlineBlocks) =
    -- Add space
    T.unwords $ map renderInlineBlock inlineBlocks

renderBulletPoint :: Start -> [Block] -> [Text]
renderBulletPoint (Start startNum) blocks =
    let renderedStart = T.replicate (startNum - 1) "\t" <> "- "
    in foldr (\block acc ->
        let rendered = case block of
                -- Filtering codeblock and table since it cannot be rendered as bulletpoint
                -- Special case on bullet point
                CODE_BLOCK codeName codeSnippet ->
                    renderCodeblock codeName codeSnippet
                TABLE tableName tableContent ->
                    renderTable tableName tableContent
                BULLET_POINT (Start num) blocks' ->
                    renderBulletPoint (Start (num + startNum)) blocks'
                others ->
                    map (\line -> renderedStart <> line) $ renderBlock others
        in rendered <> acc
        ) mempty blocks

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

 -- Does commonmark support math expressions?
renderInlineBlock :: InlineBlock -> Text
renderInlineBlock = \case
    CODE_NOTATION text   -> "`" <> text <> "`"
    MATH_EXPRESSION text -> "`" <> text <> "`"
    ITEM style segments  ->
        let renderedSegments = foldr ( (<>) . renderSegment) mempty segments
        in case style of
            Bold                    -> "**" <> renderedSegments <> "**"
            Italic                  -> "_" <> renderedSegments <> "_"
            StrikeThrough           -> "~~" <> renderedSegments <> "~~"
            NoStyle                 -> renderedSegments
            CustomStyle customStyle -> renderWithStyle customStyle renderedSegments
            UserStyle   _           -> "**" <> renderedSegments <> "**"
  where
    -- (TODO) Look into how you can apply header fonts
    renderWithStyle :: StyleData -> Text -> Text
    renderWithStyle (StyleData _ isBold isItalic isStrikeThrough) text =
        foldr (\(hasStyle, apply) acc -> if hasStyle then apply acc else acc) text $
            zip
                [isBold, isStrikeThrough, isItalic]
                [withBold, withStrikeThrough, withItalic]
    withBold :: Text -> Text
    withBold txt          = "**" <> txt <> "**"

    withStrikeThrough :: Text -> Text
    withStrikeThrough txt = "~~" <> txt <> "~~"

    withItalic :: Text -> Text
    withItalic txt        = "_" <> txt <> "_"

renderCodeblock :: CodeName -> CodeSnippet -> [Text]
renderCodeblock (CodeName name) (CodeSnippet snippet) =
    [name] <> ["```"] <> snippet <> ["```"]

renderTable :: TableName -> TableContent -> [Text]
renderTable (TableName name) (TableContent contents) =
    let renderedContent = fromMaybe (map T.unwords contents) renderTableM
    in [name] <> renderedContent
  where
    renderTableM :: Maybe [Text]
    renderTableM = do
        headColumn <- headMaybe contents
        rest       <- tailMaybe contents
        let headColumnNums = map T.length headColumn
        return $ [renderColumn headColumn] <> [middle headColumnNums] <> map renderColumn rest

    renderColumn :: [Text] -> Text
    renderColumn items = "|" <> foldr (\item acc -> item <> "|" <> acc) mempty items

    middle :: [Int] -> Text
    middle = foldl' (\acc num -> acc <> T.replicate num "-" <> "|") "|"
