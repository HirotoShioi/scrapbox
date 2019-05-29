{-| This module exports 'renderToCommonmarkNoOption' which renders 'Scrapbox'
AST into commonmark
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Render.Commonmark
    ( renderToCommonmarkNoOption
    , renderInlineBlock
    ) where
import           RIO

import           Data.Scrapbox.Types (Block (..), CodeName (..),
                                      CodeSnippet (..), InlineBlock (..),
                                      Level (..), ScrapText (..), Scrapbox (..),
                                      Segment (..), Start (..), Style (..),
                                      TableContent (..), TableName (..),
                                      Url (..))
import           Network.URI (parseURI, uriQuery)
import           RIO.List (foldl', headMaybe, nub, tailMaybe)
import qualified RIO.Text as T

-- | Render given 'Scrapbox' AST into commonmark
renderToCommonmarkNoOption :: Scrapbox -> Text
renderToCommonmarkNoOption (Scrapbox blocks) = T.unlines
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
    THUMBNAIL url                   -> [renderUrl mempty url]

-- | Render 'Url'
renderUrl :: Text -> Url -> Text
renderUrl name (Url url)
  | "https://www.youtube.com/" `T.isPrefixOf` url =
        maybe
            url
            (\youtubeId ->
                "[![" <> name <> "](https://img.youtube.com/vi/" <> youtubeId <> "/0.jpg)](" <> url <> ")"
            )
            (do
                uri      <- parseURI (T.unpack url)
                stripped <- T.stripPrefix "?v=" $ fromString $ uriQuery uri
                let youtubeId = T.takeWhile (`notElem` ['&', '?']) stripped
                return youtubeId
            )
  | any (`T.isSuffixOf` url)
        [ ".bmp"
        , ".gif"
        , ".jpg"
        , ".jpeg"
        , ".png"
        ]       = "![" <> name <> "](" <> url <> ")"
  | otherwise   = "[" <> name <> "]" <> "(" <> url <> ")"

-- | Render 'ScrapText'
renderScrapText :: ScrapText -> Text
renderScrapText (ScrapText inlineBlocks) = 
    adjust $ foldr ((:) . renderInlineBlock) mempty inlineBlocks
  where
    adjust :: [Text] -> Text
    adjust [] = mempty
    adjust [x] = x
    adjust (text : xs)
      | T.stripEnd text /= text = text <> adjust xs
      | otherwise               = text <> " " <> adjust xs  

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
        renderedSegments = foldr ( (<>) . renderSegment) mempty (adjustSpaces segments)
        renderedLevel    = T.replicate level "#"
    in  renderedLevel <> " " <> renderedSegments
  where
    -- [HASHTAG "a", TEXT " a"]
    adjustSpaces :: [Segment] -> [Segment]
    adjustSpaces [] = []
    adjustSpaces (h1@(HASHTAG _t1) : h2@(HASHTAG _t2) : rest) = h1 : TEXT " " : adjustSpaces (h2 : rest)
    adjustSpaces (x:xs) = x : adjustSpaces xs

-- | Render 'Segment'
renderSegment ::  Segment -> Text
renderSegment = \case
    HASHTAG text               -> "**#" <> text <> "**"
    LINK Nothing url           -> renderUrl mempty url
    LINK (Just name) url       -> renderUrl name url
    TEXT text                  -> text

 -- Does commonmark support math expressions?
renderInlineBlock :: InlineBlock -> Text
renderInlineBlock = \case
    CODE_NOTATION text   -> "`" <> text <> "`"
    MATH_EXPRESSION text -> "`" <> text <> "`"
    SPAN styles segments ->
        let renderedSegments = foldr ( (<>) . renderSegment) mempty segments
        in renderWithStyle (nub styles) renderedSegments
  where
    -- Render given text with 'StyleData'
    renderWithStyle :: [Style] -> Text -> Text
    renderWithStyle styles text = foldr apply text styles

    apply :: Style -> Text -> Text
    apply Bold text                = "**" <> text <> "**"
    apply Italic text              = "_" <> text <> "_"
    apply StrikeThrough text       = "~~" <> text <> "~~"
    apply (Sized (Level lvl)) text = applySize lvl text
    apply (UserStyle _) text       = "**" <> text <> "**"

    -- Add font size
    applySize :: Int -> Text -> Text
    applySize fontSize text = mconcat
        [ "<span style=\"font-size:"
        , tshow (fromIntegral fontSize * 0.5 :: Double) -- Might tweak the numbers
        , "em\">"
        , text
        , "</span>"
        ]

-- | Render 'CODE_BLOCK'
renderCodeblock :: CodeName -> CodeSnippet -> [Text]
renderCodeblock (CodeName name) (CodeSnippet snippet) =
    ["```" <> name] <> snippet <> ["```"]

-- | Render @TABLE@
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

    -- Render column section
    renderColumn :: [Text] -> Text
    renderColumn items = "| " <> foldr (\item acc -> item <> " | " <> acc) mempty items

    -- Render middle section
    middle :: [Int] -> Text
    middle = foldl' (\acc num -> acc <> T.replicate (num + 2) "-" <> "|") "|"
