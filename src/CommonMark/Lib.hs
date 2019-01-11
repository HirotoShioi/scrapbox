{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module CommonMark.Lib
    ( -- * Parser
      parseNode
    , commonmarkToMarkdown
    , commonmarkToScrapbox
    -- * Parse option
    , ParseOption
    , optDefault
    , optSectionHeader
    ) where

import           RIO                    hiding (link)

import           CMark                  (Node (..), NodeType (..), Title, Url,
                                         commonmarkToNode, optHardBreaks,
                                         optSafe)
import           Data.List.Split        (splitWhen)
import qualified RIO.Text               as T

import           Constructors           (blockQuote, bold, bulletList,
                                         codeBlock, codeNotation, header,
                                         italic, link, markdown, noStyle,
                                         paragraph, text, thumbnail)
import           Render                 (renderPretty)
import           Types                  (Block (..), Context (..),
                                         Markdown (..), Segment, concatContext,
                                         concatScrapText)

import           CommonMark.TableParser (commonMarkTableToTable, parseTable)

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

-- | Parser option which user can provide
data ParseOption
    = Default
    -- ^ Will convert CommonMark to ScrapBox format as is
    | SectionHeader
    -- ^ Will add linebreak before header to make it easier to see

optDefault :: ParseOption
optDefault = Default

optSectionHeader :: ParseOption
optSectionHeader = SectionHeader

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

-- | Convert given common mark into 'Markdown'
commonmarkToMarkdown :: ParseOption -> Text -> Markdown
commonmarkToMarkdown parseOption cmark =
    let options = [optSafe, optHardBreaks]
        node = commonmarkToNode options cmark
    in parseNode parseOption node

-- | Convert given common mark into Scrapbox markdown format
commonmarkToScrapbox :: ParseOption -> Text -> Text
commonmarkToScrapbox parseOption cmark = renderPretty $ commonmarkToMarkdown parseOption cmark

parseNode :: ParseOption -> Node -> Markdown
parseNode Default node       = markdown $ toBlocks node
parseNode SectionHeader node = markdown $ applyLinebreak $ toBlocks node

--------------------------------------------------------------------------------
-- Conversion logic from Node to Block
--------------------------------------------------------------------------------

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
    PARAGRAPH                    -> parseParagraph contents
    DOCUMENT                     -> concatMap toBlocks contents
    HEADING headerNum            -> [toHeader headerNum contents]
    EMPH                         -> [paragraph [italic (concatMap toSegments contents)]]
    STRONG                       -> [paragraph [bold (concatMap toSegments contents)]]
    TEXT textContent             -> [paragraph [noStyle [text textContent]]]
    CODE codeContent             -> [paragraph [noStyle [codeNotation codeContent]]]
    CODE_BLOCK codeInfo code     -> [toCodeBlock codeInfo code]
    LIST _                       -> [toBulletList contents]
    ITEM                         -> concatMap toBlocks contents
    SOFTBREAK                    -> [paragraph [noStyle [text "\t"]]]
     --workaround need to pay attention
    LINEBREAK                    -> [paragraph [noStyle [text "\n"]]]
    LINK url title               -> [paragraph [noStyle [toLink contents url title]]]
    HTML_BLOCK htmlContent       -> [codeBlock "html" htmlContent]
    IMAGE url _                  -> [thumbnail url]
    HTML_INLINE htmlContent      -> [codeBlock "html" htmlContent]
    BLOCK_QUOTE                  -> [blockQuote $ concatMap toContext contents]

    -- I have on idea what these are,
    -- Use placeholder for now. Need to investigate what these actually are
    CUSTOM_INLINE _ _            -> parseParagraph contents
    CUSTOM_BLOCK _ _             -> parseParagraph contents
    THEMATIC_BREAK               -> [paragraph [noStyle [text "\n"]]]

-- | Convert 'Node' into list of 'Segment'
toSegments :: Node -> [Segment]
toSegments (Node _ nodeType contents) = case nodeType of
    TEXT textContent -> [text textContent]
    CODE codeContent -> [codeNotation codeContent]
    LINK url title   -> [toLink contents url title]
    IMAGE url title  -> [toLink contents url title]
    _                -> concatMap toSegments contents

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
    in header headerSize $ concatMap toSegments nodes

-- | Extract text from nodes
extractTextFromNodes :: [Node] -> Text
extractTextFromNodes = foldr
    (\(Node _ nodeType nodes') acc
        -> extractText nodeType <> extractTextFromNodes nodes' <> acc
    ) mempty
  where
    extractText :: NodeType -> Text
    extractText = \case
        TEXT textContent -> textContent
        CODE codeContent -> codeContent
        -- For now, we're going to ignore everything else
        _         -> mempty

-- | Construct bulletlist
toBulletList :: [Node] -> Block
toBulletList contents = bulletList $ concatMap toBlocks contents

-- | Apply LineBreak between Header section
--
-- [Header, b1, b2, b3, Header, b4, b5, b6, Header]
-- =>
-- [Header, b1, b2, b3, LineBreak, Header, b4, b5, b6, LineBreak, Header]
applyLinebreak :: [Block] -> [Block]
applyLinebreak []                               = []
applyLinebreak [b]                              = [b]
applyLinebreak (b:Header hsize hcontent:rest) =
    b : LineBreak : applyLinebreak (Header hsize hcontent : rest)
applyLinebreak (b: rest)                        = b : applyLinebreak rest

--------------------------------------------------------------------------------
-- Paragraph parsing logic
--------------------------------------------------------------------------------

-- | Parse nodes and produce either an 'Table' or 'Paragraph
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
        let filteredNodes   = splitWhen (\(Node _ nodetype _) -> nodetype == SOFTBREAK) nodes'
            extractedTexts  = map extractTextFromNodes filteredNodes
        in all (T.any (== '|')) extractedTexts

    toTable :: [Node] -> [Block]
    toTable nodes' = do
        let splittedNodes     = splitWhen (\(Node _ nodetype _) -> nodetype == SOFTBREAK) nodes'
            nodeTexts         = map extractTextFromNodes splittedNodes
        either
            (\_ -> toParagraph nodes')
            (\tableContent -> [commonMarkTableToTable tableContent])
            (parseTable nodeTexts)

    -- | Convert list of 'Node' into list of 'Blocks'
    toParagraph :: [Node] -> [Block]
    toParagraph nodes' =
        let blocks = concatMap toBlocks nodes'
            consolidatedBlocks = concatParagraph blocks
        in consolidatedBlocks

    -- | Concatenate 'Paragraph' blocks
    concatParagraph :: [Block] -> [Block]
    concatParagraph []  = []
    concatParagraph [n] = [n]
    concatParagraph (Paragraph stext1 : Paragraph stext2 : rest) =
        let concatedParagraph = Paragraph $ concatScrapText stext1 stext2
        in concatParagraph $ [concatedParagraph] <> rest
    concatParagraph (a : b : rest) = a : b : concatParagraph rest
