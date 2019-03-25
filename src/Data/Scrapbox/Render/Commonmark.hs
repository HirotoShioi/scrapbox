{-| Render commonmark
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Render.Commonmark
    ( renderToCommonmark
    ) where

import           RIO
import           RIO.List       (foldl', headMaybe, tailMaybe)
import qualified RIO.Text       as T
import           Data.Scrapbox.Types (Block (..), CodeName (..), CodeSnippet (..),
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

-- | Render 'ScrapText'
renderScrapText :: ScrapText -> Text
renderScrapText (ScrapText inlineBlocks) =
    -- Add spaces between inlineblocks to ensure each of them are rendered correctly
    T.unwords $ map renderInlineBlock inlineBlocks

-- | Render 'BULLET_POINT'
renderBulletPoint :: Start -> [Block] -> [Text]
renderBulletPoint (Start startNum) blocks =
    let renderedStart = T.replicate (startNum - 1) "\t" <> "- "
    in foldr (\block acc ->
        let rendered = case block of
                -- Filtering 'CODE_BLOCK' and 'TABLE' blocks since it cannot be
                -- rendered as bulletpoint
                CODE_BLOCK codeName codeSnippet ->
                    renderCodeblock codeName codeSnippet
                TABLE tableName tableContent ->
                    renderTable tableName tableContent
                -- Special case on 'BULLET_POINT'
                BULLET_POINT (Start num) blocks' ->
                    renderBulletPoint (Start (num + startNum)) blocks'
                others ->
                    map (\line -> renderedStart <> line) $ renderBlock others
        in rendered <> acc
        ) mempty blocks

-- | Render 'HEADING'
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

-- | Render 'Segment'
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
    -- Render given text with 'StyleData'
    renderWithStyle :: StyleData -> Text -> Text
    renderWithStyle (StyleData fontSize isBold isItalic isStrikeThrough) text =
        foldr (\(applicable, apply) acc -> if applicable then apply acc else acc) text $
            zip
                [fontSize > 0, isBold, isStrikeThrough, isItalic]
                [applySize fontSize, applyBold, applyStrikeThrough, applyItalic]
    -- Add font size
    applySize :: Int -> Text -> Text
    applySize fontSize text = mconcat
        [ "<span style=\"font-size:"
        , tshow (fromIntegral fontSize * 0.7 :: Double) -- Might tweak the numbers
        , "em\">"
        , text
        , "</span>"
        ]
    -- Add bold style
    applyBold :: Text -> Text
    applyBold txt          = "**" <> txt <> "**"
    -- Add strikethrough style
    applyStrikeThrough :: Text -> Text
    applyStrikeThrough txt = "~~" <> txt <> "~~"
    -- Add italic style
    applyItalic :: Text -> Text
    applyItalic txt        = "_" <> txt <> "_"

-- | Render 'CODE_BLOCK'
renderCodeblock :: CodeName -> CodeSnippet -> [Text]
renderCodeblock (CodeName name) (CodeSnippet snippet) =
    [name] <> ["```"] <> snippet <> ["```"]

-- | Render 'TABLE'
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
