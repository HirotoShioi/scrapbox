{-| This module exports a set of renderer functions for Commonmark
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Render.Commonmark
    ( renderToCommonmarkNoOption
    , renderInlineBlock
    , renderTable
    , renderSegment
    , exampleBlock
    , exampleBlock2
    , modifyBulletPoint
    ) where
import           RIO

import           Control.Monad.State.Strict (MonadState, modify, runState)
import           Data.Scrapbox.Types (Block (..), CodeName (..),
                                      CodeSnippet (..), InlineBlock (..),
                                      Level (..), ScrapText (..), Scrapbox (..),
                                      Segment (..), Start (..), Style (..),
                                      TableContent (..), TableName (..),
                                      Url (..))
import           Network.URI (parseURI, uriQuery)
import           RIO.List (foldl', headMaybe, maximumMaybe, nub, tailMaybe)
import qualified RIO.Text as T

-- | Render given 'Scrapbox' AST into commonmark
renderToCommonmarkNoOption :: Scrapbox -> Text
renderToCommonmarkNoOption (Scrapbox blocks) = T.unlines
    . concatMap renderBlock
    . addLineBreaks
    . modifyBulletPoint
    $ blocks
  where
    addLineBreaks :: [Block] -> [Block]
    addLineBreaks []               = []
    addLineBreaks [x]              = [x]
    addLineBreaks (LINEBREAK:xs)   = LINEBREAK : addLineBreaks xs
    addLineBreaks (x:LINEBREAK:xs) = x : LINEBREAK : addLineBreaks xs
    addLineBreaks (x:xs)           = x : LINEBREAK : addLineBreaks xs

-- | Render @Block@
renderBlock :: Block -> [Text]
renderBlock = \case
    LINEBREAK                       -> [""]
    BLOCK_QUOTE scraptext           -> [">" <> renderScrapText scraptext]
    BULLET_POINT _start blocks      -> renderBulletPointUnsafe (Start 0) blocks
    CODE_BLOCK codeName codeSnippet -> renderCodeblock codeName codeSnippet
    HEADING level segments          -> [renderHeading level segments]
    PARAGRAPH scraptext             -> [renderScrapText scraptext]
    TABLE tableName tableContent    -> renderTable tableName tableContent
    THUMBNAIL url                   -> [renderUrl mempty url]

-- | Render @Url@
renderUrl :: Text -> Url -> Text
renderUrl name (Url url)
  | "https://www.youtube.com/" `T.isPrefixOf` url =
        maybe
            url
            (\youtubeId -> mconcat
                [ "[!["
                , name
                , "](https://img.youtube.com/vi/"
                , youtubeId
                , "/0.jpg)]("
                , url
                , ")"
                ]
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
    adjust $ foldl' (\acc inline -> acc <> [renderInlineBlock inline]) mempty (addSpacesHash inlineBlocks)
  where
    adjust :: [Text] -> Text
    adjust [] = mempty
    adjust [x] = x
    adjust (text : xs)
      | T.stripEnd text /= text || T.null text = text <> adjust xs
      | otherwise                              = text <> " " <> adjust xs

    addSpacesHash :: [InlineBlock] -> [InlineBlock]
    addSpacesHash [] = []
    addSpacesHash (SPAN [] (HASHTAG tag : rest) : xs) =
        SPAN [] (TEXT " " : HASHTAG tag : rest) : xs
    addSpacesHash others = others

-- | Render @BULLET_POINT@
--
-- Do not export this.
-- This function needs to be used by calling from renderToCommonmarkNoOption
renderBulletPointUnsafe :: Start -> [Block] -> [Text]
renderBulletPointUnsafe (Start num) = foldl' (\acc block->
        let spaces   = T.replicate num " "
            rendered = case block of
                -- Filtering 'CODE_BLOCK' and 'TABLE' blocks since it cannot be
                -- rendered as bulletpoint
                CODE_BLOCK codeName codeSnippet ->
                    addSpaces acc $ renderCodeblock codeName codeSnippet
                TABLE tableName tableContent ->
                    addSpaces acc $ renderTable tableName tableContent
                -- Special case on 'BULLET_POINT'
                BULLET_POINT _s blocks' ->
                    renderBulletPointUnsafe (Start $ num + 1) blocks'
                others -> map (\t -> spaces <> "- " <> t) $ renderBlock others
        in acc <> rendered
        ) mempty
  where
    addSpaces :: [Text] -> [Text] -> [Text]
    addSpaces acc content
      | null acc  = content
      | otherwise = [""] <> content

-- | Render @HEADING@
renderHeading :: Level -> [Segment] -> Text
renderHeading (Level headingNum) segments =
    let level = case headingNum of
                4 -> 1
                3 -> 2
                2 -> 3
                1 -> 4
                _ -> 4
        renderedSegments = foldl'
            (\acc segment -> acc <> renderSegment segment)
            mempty
            (adjustSpaces segments)
        renderedLevel    = T.replicate level "#"
    in  renderedLevel <> " " <> renderedSegments
  where
    -- [HASHTAG "a", TEXT " a"]
    adjustSpaces :: [Segment] -> [Segment]
    adjustSpaces [] = []
    adjustSpaces (h1@(HASHTAG _t1) : h2@(HASHTAG _t2) : rest) =
        h1 : TEXT " " : adjustSpaces (h2 : rest)
    adjustSpaces (x:xs) = x : adjustSpaces xs

-- | Render @Segment@
renderSegment ::  Segment -> Text
renderSegment = \case
    HASHTAG text               -> "#" <> text <> ""
    LINK Nothing url           -> renderUrl mempty url
    LINK (Just name) url       -> renderUrl name url
    TEXT text                  -> text

-- | Render @InlineBlock@
renderInlineBlock :: InlineBlock -> Text
renderInlineBlock = \case
    CODE_NOTATION text   -> "`" <> text <> "`"
    MATH_EXPRESSION text -> "`" <> text <> "`"
    SPAN styles segments ->
        let renderedSegments = foldl'
                (\acc segment -> acc <> renderSegment segment)
                mempty
                segments
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

-- | Render @CODE_BLOCK@
renderCodeblock :: CodeName -> CodeSnippet -> [Text]
renderCodeblock (CodeName name) (CodeSnippet snippet) =
    ["```" <> name] <> snippet <> ["```"]

-- | Render @TABLE@
renderTable :: TableName -> TableContent -> [Text]
renderTable (TableName name) (TableContent [])       = [name]
renderTable (TableName name) (TableContent contents)
  | all null contents = [name]
  | otherwise =
    let alignedContents = alignTable contents
        renderedContent = fromMaybe
            (map T.unwords alignedContents)
            (renderTableM alignedContents)
    in if T.null name
        then renderedContent
        else [name] <> [""] <> renderedContent
  where
    renderTableM :: [[Text]] -> Maybe [Text]
    renderTableM cs = do
        headColumn <- headMaybe cs
        rest       <- tailMaybe cs
        let headColumnNums = map T.length headColumn
        return $ [renderColumn headColumn]
            <> [middle headColumnNums]
            <> map renderColumn rest

    -- Render column section
    renderColumn :: [Text] -> Text
    renderColumn items = "| " <> foldr (\item acc -> item <> " | " <> acc) mempty items

    -- Render middle section
    middle :: [Int] -> Text
    middle = foldl' (\acc num -> acc <> T.replicate (num + 2) "-" <> "|") "|"

    alignTable :: [[Text]] -> [[Text]]
    alignTable rows = maybe rows (align rows) (maximumMaybe $ map length rows)

    align :: [[Text]] -> Int -> [[Text]]
    align rows maxRow = foldl' (\acc row -> if length row < maxRow
        then acc <> [row <> replicate (maxRow - length row) ""]
        else acc <> [row]
        ) mempty rows

filterEmpty :: [Block] -> [Block]
filterEmpty [] = []
filterEmpty (BULLET_POINT s bs : xs) =
    if null (filterEmpty bs)
        then filterEmpty xs
        else BULLET_POINT s (filterEmpty bs) : filterEmpty xs
filterEmpty (x : xs)  = x : filterEmpty xs

modifyBulletPoint :: [Block] -> [Block]
modifyBulletPoint blocks = filterEmpty $ foldl' (\acc block -> case block of
        BULLET_POINT s bs ->
            let (stay, out) = runState (extract bs) []
            in acc <> [BULLET_POINT s stay] <> out
        _others -> acc <> [block]
    ) mempty blocks
  where
    extract :: (MonadState [Block] m) => [Block] -> m [Block]
    extract = foldM (\acc block -> case block of
        t@(TABLE (TableName name) (TableContent contents)) -> 
            if null contents || all null contents
            then do
                let paragraph = PARAGRAPH $ ScrapText [SPAN [] [TEXT name]]
                return (acc <> [paragraph])
            else modify (<> [t]) >>  return acc
        c@(CODE_BLOCK _n _s) -> modify (<> [c]) >>  return acc
        BULLET_POINT s' bs   -> do
            rest <- extract bs
            return (acc <> [BULLET_POINT s' rest])
        others               -> return (acc <> [others])
        ) mempty

exampleBlock :: Block
exampleBlock = BULLET_POINT ( Start 2 )
    [ HEADING ( Level 2 ) [ TEXT "World" ]
    , CODE_BLOCK (CodeName "codename") (CodeSnippet [])
    , BULLET_POINT ( Start 3 )
        [ BULLET_POINT ( Start 1 )
            [ HEADING ( Level 4 ) [ TEXT "Hello" ]
            , LINEBREAK
            , TABLE ( TableName "tableName" )
                ( TableContent [ [""] ] )
            , CODE_BLOCK (CodeName "codename") (CodeSnippet [])
            ]
        ]
    ]

exampleBlock2 :: Block
exampleBlock2 = BULLET_POINT ( Start 3 )
    [ HEADING ( Level 2 ) [ TEXT "hello" ]
    , BULLET_POINT ( Start 2 )
        [ CODE_BLOCK ( CodeName "codeName" ) ( CodeSnippet [] )
        , LINEBREAK
        , CODE_BLOCK ( CodeName "codeName" ) ( CodeSnippet [] )
        ]
    ]
