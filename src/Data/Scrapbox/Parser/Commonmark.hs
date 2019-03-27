{-| This module exposes parser functions. You must provide 'ParseOption'
which is either 'optDefault' or 'optSectionHeading'

To parse given Commonmark into 'Scrapbox' AST, use 'commonmarkToNode'.

To parse given CommnMark and convert into 'Scrapbox' format,
use 'commonmarkToScrapbox'.
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Parser.Commonmark
    ( -- * Parser
      parseCommonmark
    ) where

import           RIO                                         hiding (link)

import           CMark                                       (Node (..),
                                                              NodeType (..),
                                                              PosInfo (..),
                                                              Title, Url,
                                                              optHardBreaks,
                                                              optSafe)
import qualified CMark                                       as C
import           Data.List.Split                             (splitWhen)
import qualified RIO.Text                                    as T

import           Data.Scrapbox.Constructors                  (blockQuote, bold,
                                                              bulletPoint,
                                                              codeBlock,
                                                              codeNotation,
                                                              heading, italic,
                                                              link, noStyle,
                                                              paragraph,
                                                              scrapbox, text,
                                                              thumbnail)
import           Data.Scrapbox.Types                         as S (Block (..), InlineBlock (..),
                                                                   Scrapbox (..),
                                                                   Segment (..),
                                                                   concatInline,
                                                                   concatScrapText)

import           Data.Scrapbox.Parser.Commonmark.TableParser (parseTable)

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

-- | Convert given common mark into 'Scrapbox' AST
parseCommonmark :: Text -> Scrapbox
parseCommonmark cmark =
    let options = [optSafe, optHardBreaks]
        node = C.commonmarkToNode options cmark
    in parse node

-- | Apply linebreak after TABLE and CODE_BLOCK if there's BULLET_POINT right after it
-- this prevents the weird rendering to occur
parse :: Node -> Scrapbox
parse node =  scrapbox $ format $ convertToBlocks [node]
  where
    format :: [Block] -> [Block]
    format [] = []
    format (t@(TABLE _ _): b@(BULLET_POINT _ _) : rest) =
        t : S.LINEBREAK : b : format rest
    format (c@(S.CODE_BLOCK _ _): b@(BULLET_POINT _ _) : rest) =
        c : S.LINEBREAK : b : format rest
    format (x:xs) = x : format xs

--------------------------------------------------------------------------------
-- Conversion logic from Node to Block
--------------------------------------------------------------------------------

-- | Compute the diff between the blocks and apply LINEBREAK accordingly
convertToBlocks :: [Node] -> [Block]
convertToBlocks []  = []
convertToBlocks [x] = toBlocks x
convertToBlocks ( node1@(Node (Just (PosInfo _ _ end _)) _ _) 
                : node2@(Node (Just (PosInfo start _ _ _)) _ _) 
                : rest
                ) = do
    let diff = start - end - 1
    if diff >= 1
        then toBlocks node1 <> replicate diff S.LINEBREAK <> convertToBlocks (node2 : rest)
        else toBlocks node1 <> convertToBlocks (node2 : rest)
convertToBlocks (x:xs) = toBlocks x <> convertToBlocks xs

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
toBlocks :: Node -> [Block]
toBlocks (Node _ nodeType contents) = case nodeType of
    C.PARAGRAPH                -> parseParagraph contents
    C.DOCUMENT                 -> convertToBlocks contents
    C.HEADING headingNum       -> [toHeading headingNum contents]
    C.EMPH                     -> [paragraph [italic (concatMap toSegments contents)]]
    C.STRONG                   -> [paragraph [bold (concatMap toSegments contents)]]
    C.TEXT textContent         -> [paragraph [noStyle [text textContent]]]
    C.CODE codeContent         -> [paragraph [codeNotation codeContent]]
    C.CODE_BLOCK codeInfo code -> [toCodeBlock codeInfo (T.lines code)]
    C.LIST _                   -> [toBulletPoint contents]
    C.ITEM                     -> convertToBlocks contents
    C.SOFTBREAK                -> [paragraph [noStyle [text ""]]]
     -- Workaround need to pay attention
    C.LINEBREAK                -> [paragraph [noStyle [text "\n"]]]
    C.LINK url title           -> [paragraph [noStyle [toLink contents url title]]]
    C.HTML_BLOCK htmlContent   -> [codeBlock "html" (T.lines htmlContent)]
    C.IMAGE url _              -> [thumbnail url]
    C.HTML_INLINE htmlContent  -> [codeBlock "html" (T.lines htmlContent)]
    C.BLOCK_QUOTE              -> [blockQuote $ concatMap toInlineBlock contents]
    -- I have on idea what these are,
    -- Use placeholder for now. Need to investigate what these actually are
    C.CUSTOM_INLINE _ _        -> parseParagraph contents
    C.CUSTOM_BLOCK _ _         -> parseParagraph contents
    C.THEMATIC_BREAK           -> [paragraph [noStyle [text "\n"]]]

-- | Convert 'Node' into list of 'Segment'
toSegments :: Node -> [Segment]
toSegments (Node _ nodeType contents) = case nodeType of
    C.TEXT textContent -> [text textContent]
    -- CODE codeContent -> [text codeContent] What's going to happen?
    C.LINK url title   -> [toLink contents url title]
    IMAGE url title    -> [toLink contents url title]
    _                  -> concatMap toSegments contents

-- | Convert 'Node' into list of 'InlineBlock'
-- Need state monad to inherit style from parent node
toInlineBlock :: Node -> [InlineBlock]
toInlineBlock = concatInline . convertToInlineBlock
  where
    convertToInlineBlock :: Node -> [InlineBlock]
    convertToInlineBlock (Node _ nodeType contents) = case nodeType of
        EMPH               -> [italic (concatMap toSegments contents)]
        STRONG             -> [bold (concatMap toSegments contents)]
        C.TEXT textContent -> [noStyle [text textContent]]
        CODE codeContent   -> [codeNotation codeContent]
        C.LINK url title   -> [noStyle [toLink contents url title]]
        IMAGE url title    -> [noStyle [toLink contents url title]]
        _                  -> concatMap toInlineBlock contents

-- | Convert given LINK into 'Segment'
toLink :: [Node] -> CMark.Url -> Title -> Segment
toLink nodes url title
    | T.null title = mkLink url (extractTextFromNodes nodes)
    | null nodes   = mkLink url title
    | otherwise    = mkLink url title
  where
    mkLink :: CMark.Url -> Text -> Segment
    mkLink url' title'
        | T.null title' = link Nothing url'
        | otherwise     = link (Just title') url'

-- | Convert to 'CODE_BLOCK'
toCodeBlock :: Text -> [Text] -> Block
toCodeBlock codeInfo code
    | T.null codeInfo = codeBlock "code" code
    | otherwise       = codeBlock codeInfo code

-- | Convert to 'HEADING'
toHeading :: Int -> [Node] -> Block
toHeading headingNum nodes =
    -- Headers in scrapbox are opposite of what commonmark are
    let level = case headingNum of
                    1 -> 4
                    2 -> 3
                    3 -> 2
                    4 -> 1
                    _ -> 1
    in heading level $ concatMap toSegments nodes

-- | Extract text from nodes
extractTextFromNodes :: [Node] -> Text
extractTextFromNodes = foldr
    (\(Node _ nodeType nodes') acc ->
        extractText nodeType <> extractTextFromNodes nodes' <> acc
    ) mempty
  where
    extractText :: NodeType -> Text
    extractText = \case
        C.TEXT textContent -> textContent
        CODE codeContent   -> "`" <> codeContent <> "`"
        -- For now, we're going to ignore everything else
        _         -> mempty

-- | Construct 'BULLET_POINT'
toBulletPoint :: [Node] -> Block
toBulletPoint nodes = bulletPoint 1 $ convertToBlocks nodes

--------------------------------------------------------------------------------
-- Paragraph parsing logic
--------------------------------------------------------------------------------

-- | Parse nodes and produce either an 'TABLE' or 'PARAGRAPH'
--
-- CMark parses Table as an list of Paragraphs
-- So we need to parse it on our own.
--
-- Hiding functions it so that they will not be misused.
parseParagraph :: [Node] -> [Block]
parseParagraph nodes = if isTable nodes
    then toTable nodes
    else toParagraph nodes
  where
    isTable :: [Node] -> Bool
    isTable nodes' =
            any (\(Node _ nodetype _) -> nodetype == SOFTBREAK) nodes'
         && length nodes >= 2 -- Table needs at least a header and seperator to be valid
         && hasSymbols nodes

    -- Each of the element should have '|' symbol to be an valid table
    hasSymbols :: [Node] -> Bool
    hasSymbols nodes' =
        let filteredNodes  =
                splitWhen (\(Node _ nodetype _) -> nodetype == SOFTBREAK) nodes'
            extractedTexts = map extractTextFromNodes filteredNodes
        in all (T.any (== '|')) extractedTexts

    toTable :: [Node] -> [Block]
    toTable nodes' =
        let splittedNodes = splitWhen (\(Node _ nodetype _) -> nodetype == SOFTBREAK) nodes'
            nodeTexts     = map extractTextFromNodes splittedNodes
        in either
            (\_ -> toParagraph nodes')
            (: [])
            (parseTable nodeTexts)

    -- | Convert list of 'Node' into list of 'Block'
    toParagraph :: [Node] -> [Block]
    toParagraph = concatParagraph . convertToBlocks

    -- | Concatenate 'PARAGRAPH' blocks
    concatParagraph :: [Block] -> [Block]
    concatParagraph []  = []
    concatParagraph [n] = [n]
    concatParagraph (S.PARAGRAPH stext1 : S.PARAGRAPH stext2 : rest) =
        let concatedParagraph = S.PARAGRAPH $ concatScrapText stext1 stext2
        in concatParagraph $ [concatedParagraph] <> rest
    concatParagraph (a : b : rest) = a : b : concatParagraph rest