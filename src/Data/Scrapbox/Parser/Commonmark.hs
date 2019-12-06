{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines a parser for Commnonmark.
module Data.Scrapbox.Parser.Commonmark
  ( -- * Parser
    parseCommonmarkNoOption,
  )
where

import CMarkGFM
  ( CMarkExtension,
    Node (..),
    NodeType (..),
    PosInfo (..),
    Title,
    extAutolink,
    extStrikethrough,
    extTable,
    extTagfilter,
    optHardBreaks,
  )
import qualified CMarkGFM as C
import Data.Scrapbox.Constructors
  ( blockQuote,
    bulletPoint,
    codeBlock,
    codeNotation,
    heading,
    link,
    paragraph,
    scrapbox,
    span,
    text,
  )
import Data.Scrapbox.Types as S
  ( Block (..),
    InlineBlock (..),
    Scrapbox,
    Segment (..),
    Style (..),
    TableContent (..),
    TableName (..),
    concatInline,
  )
import RIO hiding (link, span)
import qualified RIO.Text as T

--------------------------------------------------------------------------------
-- Exposed interface
--------------------------------------------------------------------------------

-- Reminder: This module is not intended to auto fix the invalid sytaxes
-- (i.e. This is not an AI that auto completes given common mark text)
--
-- So if you provide an invalid common mark, you're going to get invalid scrapbox
-- page

--- It would be nice if these functions are wrapped in Either like this:
--  newtype Parser a = Parser (Either ParserException a)

-- | Convert given common mark into 'Scrapbox' structured tree
parseCommonmarkNoOption :: Text -> Scrapbox
parseCommonmarkNoOption cmark =
  let options = [optHardBreaks]
      node = C.commonmarkToNode options exts cmark
   in parse node

exts :: [CMarkExtension]
exts =
  [ extStrikethrough,
    extTable,
    extAutolink,
    extTagfilter
  ]

-- | Apply linebreak after TABLE and CODE_BLOCK if there's BULLET_POINT right
-- after it.
--
-- this prevents the weird rendering to occur
parse :: Node -> Scrapbox
parse node = scrapbox $ format $ convertToBlocks [node]
  where
    format :: [Block] -> [Block]
    format [] = []
    format (t@(S.TABLE _ _) : b@(BULLET_POINT _ _) : rest) =
      t : S.LINEBREAK : b : format rest
    format (c@(S.CODE_BLOCK _ _) : b@(BULLET_POINT _ _) : rest) =
      c : S.LINEBREAK : b : format rest
    format (x : xs) = x : format xs

--------------------------------------------------------------------------------
-- Conversion logic from Node to Block
--------------------------------------------------------------------------------

-- | Compute the diff between the @Nodes@ and apply @LINEBREAK@ accordingly
convertToBlocks :: [Node] -> [Block]
convertToBlocks [] = []
convertToBlocks [x] = toBlocks x
convertToBlocks
  ( node1@(Node (Just (PosInfo _ _ end _)) _ _) :
      node2@(Node (Just (PosInfo start _ _ _)) _ _) :
      rest
    ) = do
    let diff = start - end - 1
    if diff >= 1
      then
        mconcat
          [ toBlocks node1,
            replicate diff S.LINEBREAK,
            convertToBlocks (node2 : rest)
          ]
      else toBlocks node1 <> convertToBlocks (node2 : rest)
convertToBlocks (x : xs) = toBlocks x <> convertToBlocks xs

toBlocks :: Node -> [Block]
toBlocks = toBlocks' mempty

-- | Convert 'Node' into list of 'Block'
--
-- Still unfinished
--
-- Need to pay attention on the implementation of these NodeTypes
-- BLOCK_QUOTE
-- CODE_BLOCK
-- DOCUMENT
-- HEADING
-- LINEBREAK
-- LIST
-- PARAGRAPH
-- SOFTBREAK
--
-- Others like STRONG and TEXT have PARAGRAPH as an parent node so it is very important
-- that the toInlineBlock is implemented correctly.
toBlocks' :: [Style] -> Node -> [Block]
toBlocks' styles (Node _ nodeType contents) = case nodeType of
  C.PARAGRAPH ->
    [paragraph $ concatMap (toInlineBlock styles) contents]
  C.DOCUMENT -> convertToBlocks contents
  C.HEADING headingNum -> [toHeading headingNum contents]
  C.EMPH ->
    [paragraph $ concatMap (toInlineBlock (Italic : styles)) contents]
  C.STRONG ->
    [paragraph $ concatMap (toInlineBlock (Bold : styles)) contents]
  C.STRIKETHROUGH ->
    [paragraph $ concatMap (toInlineBlock (StrikeThrough : styles)) contents]
  C.TEXT textContent -> [paragraph [SPAN styles [S.TEXT textContent]]]
  C.CODE codeContent -> [paragraph [codeNotation codeContent]]
  C.CODE_BLOCK codeInfo code -> [toCodeBlock codeInfo (T.lines code)]
  C.LIST _ -> [toBulletPoint contents]
  C.ITEM -> convertToBlocks contents
  C.SOFTBREAK -> [paragraph [span [] [text ""]]]
  -- Workaround need to pay attention
  C.LINEBREAK -> [paragraph [span [] [text "\n"]]]
  C.LINK url title ->
    [paragraph [span styles [toLink contents url title]]]
  C.HTML_BLOCK htmlContent -> [codeBlock "html" (T.lines htmlContent)]
  C.IMAGE url _title ->
    [paragraph [span styles [toLink contents url (extractTextFromNodes contents)]]]
  C.HTML_INLINE _htmlContent -> []
  C.BLOCK_QUOTE ->
    [blockQuote $ concatMap (toInlineBlock styles) contents]
  C.TABLE _ -> [toTable contents]
  -- place holder
  C.TABLE_ROW ->
    [paragraph $ concatMap (toInlineBlock styles) contents]
  C.TABLE_CELL ->
    [paragraph $ concatMap (toInlineBlock styles) contents]
  -- I have on idea what these are,
  -- Use placeholder for now. Need to investigate what these actually are
  C.CUSTOM_INLINE _ _ ->
    [paragraph $ concatMap (toInlineBlock styles) contents]
  C.CUSTOM_BLOCK _ _ ->
    [paragraph $ concatMap (toInlineBlock styles) contents]
  C.THEMATIC_BREAK -> [paragraph [span [] [text "\n"]]]

-- | Convert 'Node' into list of 'InlineBlock'
-- Need state monad to inherit style from parent node
toInlineBlock :: [Style] -> Node -> [InlineBlock]
toInlineBlock styles node = concatInline $ convertToInlineBlock node
  where
    convertToInlineBlock :: Node -> [InlineBlock]
    convertToInlineBlock (Node _ nodeType contents) = case nodeType of
      C.EMPH -> concatMap (toInlineBlock (Italic : styles)) contents
      C.STRONG -> concatMap (toInlineBlock (Bold : styles)) contents
      C.STRIKETHROUGH -> concatMap (toInlineBlock (StrikeThrough : styles)) contents
      C.TEXT textContent -> [SPAN styles [S.TEXT textContent]]
      C.CODE codeContent -> [codeNotation codeContent]
      C.LINK url title -> withStyle [toLink contents url title]
      C.IMAGE url _title -> withStyle [toLink contents url (extractTextFromNodes contents)]
      _ -> concatMap (toInlineBlock styles) contents
    withStyle :: [Segment] -> [InlineBlock]
    withStyle segments = [span styles segments]

-- | Convert to @CODE_BLOCK@
toCodeBlock :: Text -> [Text] -> Block
toCodeBlock codeInfo code
  | T.null codeInfo = codeBlock "code" code
  | otherwise = codeBlock codeInfo code

-- | Convert to @HEADING@
toHeading :: Int -> [Node] -> Block
toHeading headingNum nodes =
  -- Headers in scrapbox are opposite of what commonmark are
  let level = case headingNum of
        1 -> 4
        2 -> 3
        3 -> 2
        4 -> 1
        _ -> 1
   in heading level (concatMap toSegments nodes)
  where
    toSegments :: Node -> [Segment]
    toSegments (Node _ nodeType contents) = case nodeType of
      C.TEXT textContent -> [text textContent]
      C.CODE codeContent -> [text codeContent]
      C.LINK url title -> [toLink contents url title]
      -- C.HTML_INLINE htmlContent -> [text htmlContent]
      C.IMAGE url _title -> [toLink contents url (extractTextFromNodes contents)]
      _ -> concatMap toSegments contents

-- | Construct 'BULLET_POINT'
toBulletPoint :: [Node] -> Block
toBulletPoint nodes = bulletPoint 1 $ convertToBlocks nodes

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Extract text from nodes
extractTextFromNodes :: [Node] -> Text
extractTextFromNodes =
  foldl'
    ( \acc (Node _ nodeType nodes') ->
        acc <> extractText nodeType <> extractTextFromNodes nodes'
    )
    mempty
  where
    extractText :: NodeType -> Text
    extractText = \case
      C.TEXT textContent -> textContent
      C.CODE codeContent -> "`" <> codeContent <> "`"
      -- For now, we're going to ignore everything else
      _ -> mempty

-- | Convert given LINK into 'Segment'
toLink :: [Node] -> C.Url -> Title -> Segment
toLink nodes url title
  | T.null title = mkLink (extractTextFromNodes nodes)
  | null nodes = mkLink title
  | otherwise = mkLink title
  where
    mkLink :: Text -> Segment
    mkLink title'
      | T.null title' = link Nothing url
      | otherwise = link (Just title') url

toTable :: [Node] -> Block
toTable nodes = S.TABLE (TableName "table") (TableContent getTableContents)
  where
    getTableContents :: [[Text]]
    getTableContents =
      foldl'
        ( \acc (Node _ nodeType contents) -> case nodeType of
            TABLE_ROW -> acc <> [extractCells contents]
            _others -> acc
        )
        mempty
        nodes
    extractCells :: [Node] -> [Text]
    extractCells =
      foldl'
        ( \acc (Node _ nodeType contents) -> case nodeType of
            TABLE_CELL -> acc <> [extractTextFromNodes contents]
            _others -> acc
        )
        mempty
