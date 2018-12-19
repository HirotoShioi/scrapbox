{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module CommonMark
    ( test
    , testHeader
    , testWith
    , testNestedList    
    , toBlocks
    , commonmarkToMarkdown
    , commonmarkToScrapbox
    ) where

import           RIO hiding (link)

import           CMark
import qualified RIO.Text as T

import           Constructors
import           Render
import           Types

-- | Test data for example.md
test :: IO Node
test = testWith "./docs/example.md"

-- | Test data for Header
testHeader :: IO Node
testHeader = testWith "./docs/headers.md"

testNestedList :: IO Node
testNestedList = testWith "./docs/nestedList.md"

testWith :: FilePath -> IO Node
testWith filePath = do 
    markDown <- readFileUtf8 filePath
    let options = [optSafe, optHardBreaks]
    let parsed = commonmarkToNode options markDown
    return parsed

-- Reminder: This module is not intended to auto fix the invalid sytaxes
-- (i.e. This is not an AI that auto completes given common mark text)
--
-- So if you provide an invalid common mark, you're going to get invalid scrapbox
-- page

--- It would be nice if these functions are wrapped in Either like this:
--  newtype Parser a = Parser (Either ParserException a)

-- | Convert given common mark into 'Markdown'
commonmarkToMarkdown :: Text -> Markdown
commonmarkToMarkdown cmark =
    let options = [optSafe, optHardBreaks]
        parsed = commonmarkToNode options cmark
    in markdown $ toBlocks parsed

-- | Convert given common mark into Scrapbox markdown format
commonmarkToScrapbox :: Text -> Text
commonmarkToScrapbox = renderPretty . commonmarkToMarkdown

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
-- that the toContext is implemented correctly.
toBlocks :: Node -> [Block]
toBlocks (Node _ nodeType contents) = case nodeType of
    PARAGRAPH                    -> toParagraph contents
    DOCUMENT                     -> concatMap toBlocks contents
    HEADING headerNum            -> [toHeader headerNum contents]
    EMPH                         -> [paragraph [italic (concatMap toSegments contents)]]
    STRONG                       -> [paragraph [bold (concatMap toSegments contents)]]
    TEXT textContent             -> [paragraph [noStyle [text textContent]]]
    CODE codeContent             -> [paragraph [noStyle [codeNotation codeContent]]]
    CODE_BLOCK codeInfo code     -> [toCodeBlock codeInfo code]
    LIST _                       -> [toBulletList contents]
    ITEM                         -> concatMap toBlocks contents
    SOFTBREAK                    -> [lineBreak]
    LINEBREAK                    -> [lineBreak]
    LINK url title               -> [paragraph [noStyle [toLink contents url title]]]
    HTML_BLOCK htmlContent       -> [codeBlock "html" htmlContent]
    IMAGE url _                  -> [thumbnail url]
    HTML_INLINE htmlContent      -> [codeBlock "html" htmlContent]
    BLOCK_QUOTE                  -> [blockQuote $ concatMap toContext contents]
    CUSTOM_INLINE onEnter onExit -> undefined -- ??
    CUSTOM_BLOCK onEnter onExit  -> undefined -- ??
    THEMATIC_BREAK               -> undefined -- ??

-- | Convert 'Node' into list of 'Segment'
toSegments :: Node -> [Segment]
toSegments (Node _ nodeType contents) = case nodeType of
    TEXT textContent -> [text textContent]
    CODE codeContent -> [codeNotation codeContent]
    LINK url title   -> [toLink contents url title]
    IMAGE url title  -> [toLink contents url title]
    -- Potentially cause infinite loop?
    _                -> concatMap toSegments contents

-- | Convert list of 'Node' into list of 'Blocks'
toParagraph :: [Node] -> [Block]
toParagraph nodes =
    let blocks = concatMap toBlocks nodes
        consolidatedBlocks = concatParagraph blocks
        withBreaks         = addBreaks consolidatedBlocks
    in withBreaks
  where
    -- Concatenate 'Paragraph' blocks
    concatParagraph :: [Block] -> [Block]
    concatParagraph []  = []
    concatParagraph [n] = [n]
    concatParagraph ((Paragraph stext1) : (Paragraph stext2) : rest) =
        let concatedParagraph = Paragraph $ concatScrapText stext1 stext2
        in concatParagraph $ [concatedParagraph] <> rest
    concatParagraph (a:b:rest) = a : b : concatParagraph rest

    -- Add breaks after Paragraph block
    -- Will fix this
    addBreaks :: [Block] -> [Block]
    addBreaks []  = []
    addBreaks (Paragraph stext:rest) = (Paragraph stext : LineBreak : addBreaks rest)
    addBreaks (a:as)                 = (a : addBreaks as)

-- | Convert 'Node' into list of 'Context'
-- Need state monad to inherit style from parent node
toContext :: Node -> [Context]
toContext = concatContext . convertToContext
  where
    convertToContext :: Node -> [Context]
    convertToContext (Node _ nodeType contents) = case nodeType of
        EMPH             -> [italic (concatMap toSegments contents)]
        STRONG           -> [bold (concatMap toSegments contents)]
        TEXT textContent -> [noStyle [text textContent]]
        CODE codeContent -> [noStyle [codeNotation codeContent]]
        LINK url title   -> [noStyle [toLink contents url title]]
        IMAGE url title  -> [noStyle [toLink contents url title]]
        -- Potentially cause infinite loop?
        _                -> concatMap toContext contents

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

-- | Convert codeblocks
toCodeBlock :: Text -> Text -> Block
toCodeBlock codeInfo code
    | T.null codeInfo = codeBlock "code" code
    | otherwise       = codeBlock codeInfo code

-- | Convert HEADER into Header
toHeader :: Int -> [Node] -> Block
toHeader headerNum nodes =
    let headerSize =
    -- Headers in scrapbox are opposite of what common markdowns are
          case headerNum of
              1 -> 4
              2 -> 3
              3 -> 2
              4 -> 1
              _ -> 1
    in Header (HeaderSize headerSize) $ concatMap toSegments nodes

-- | Extract text from nodes
extractTextFromNodes :: [Node] -> Text
extractTextFromNodes nodes = foldr
    (\(Node _ nodeType nodes') acc
        -> extractText nodeType <> extractTextFromNodes nodes' <> acc
    ) mempty nodes
  where
    extractText :: NodeType -> Text
    extractText = \case
        TEXT textContent -> textContent
        CODE codeContent -> codeContent
        -- For now, we're going to ignore everything else
        _         -> mempty

-- | Filter out LineBreak
-- Potentially can cause bugs, will fix this!
toBulletList :: [Node] -> Block
toBulletList contents = bulletList $ filter (/= LineBreak) $ concatMap toBlocks contents

-- Notes:
-- Parser does not indicate where the linebreaks had occured
-- The only way to implement it is by using the informations of 'PosInfo'
-- so we can compute the distance between the contents/sections and calculate how many
-- line breaks are needed,
-- 
-- This will be implemented in the next PR