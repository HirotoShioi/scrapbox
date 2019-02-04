{-| This module exposes parser functions. You must provide 'ParseOption'
which is either 'optDefault' or 'optSectionHeading'

To parse given CommonMark into Scrapbox parse tree, use 'commonmarkToMarkdown'.

To parse given CommnMark and convert into Scrapbox format, use 'commonmarkToScrapbox'.
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module CommonMark.Lib
    ( -- * Parser
      parseNode
    , commonmarkToScrapboxNode
    , commonmarkToScrapbox
    -- * Parse option
    , ParseOption
    , optDefault
    , optSectionHeading
    ) where

import           RIO                    hiding (link)

import           CMark                  (Node (..), NodeType (..), Title, Url,
                                         commonmarkToNode, optHardBreaks,
                                         optSafe)
import qualified CMark                  as C
import           Data.List.Split        (splitWhen)
import qualified RIO.Text               as T

import           Constructors           (blockQuote, bold, bulletPoint,
                                         codeBlock, codeNotation, heading,
                                         italic, link, noStyle, paragraph,
                                         scrapbox, text, thumbnail)
import           Render                 (renderPretty)
import           Types                  (Block (..), Context (..),
                                         Scrapbox (..), Segment, concatContext,
                                         concatScrapText)

import           CommonMark.TableParser (commonMarkTableToTable, parseTable)

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

-- | Parser option which user can provide
data ParseOption
    = Default
    -- ^ Will convert 'CommonMark' to 'Scrapbox' format as is
    | SectionHeading
    -- ^ Will add 'LINEBREAK' before heading to make it easier to see

-- | Default parse option
optDefault :: ParseOption
optDefault = Default

-- | This parse option adds 'LINEBREAK' before each 'HEADING' to make it easier to see
optSectionHeading :: ParseOption
optSectionHeading = SectionHeading

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

-- | Convert given common mark into 'Scrapbox'
commonmarkToScrapboxNode :: ParseOption -> Text -> Scrapbox
commonmarkToScrapboxNode parseOption cmark =
    let options = [optSafe, optHardBreaks]
        node = commonmarkToNode options cmark
    in parseNode parseOption node

-- | Convert given common mark into Scrapbox markdown format
commonmarkToScrapbox :: ParseOption -> Text -> Text
commonmarkToScrapbox parseOption cmark = renderPretty $ commonmarkToScrapboxNode parseOption cmark

-- | Parse given CMark 'Node' into 'Markdown'
parseNode :: ParseOption -> Node -> Scrapbox
parseNode Default node        = scrapbox $ toBlocks node
parseNode SectionHeading node = scrapbox $ applyLinebreak $ toBlocks node

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
    C.PARAGRAPH                -> parseParagraph contents
    C.DOCUMENT                 -> concatMap toBlocks contents
    C.HEADING headingNum        -> [toHeading headingNum contents]
    C.EMPH                     -> [paragraph [italic (concatMap toSegments contents)]]
    C.STRONG                   -> [paragraph [bold (concatMap toSegments contents)]]
    C.TEXT textContent         -> [paragraph [noStyle [text textContent]]]
    C.CODE codeContent         -> [paragraph [noStyle [codeNotation codeContent]]]
    C.CODE_BLOCK codeInfo code -> [toCodeBlock codeInfo code]
    C.LIST _                   -> [toBulletList contents]
    C.ITEM                     -> concatMap toBlocks contents
    C.SOFTBREAK                -> [paragraph [noStyle [text "\t"]]]
     -- Workaround need to pay attention
    C.LINEBREAK                -> [paragraph [noStyle [text "\n"]]]
    C.LINK url title           -> [paragraph [noStyle [toLink contents url title]]]
    C.HTML_BLOCK htmlContent   -> [codeBlock "html" htmlContent]
    C.IMAGE url _              -> [thumbnail url]
    C.HTML_INLINE htmlContent  -> [codeBlock "html" htmlContent]
    C.BLOCK_QUOTE              -> [blockQuote $ concatMap toContext contents]

    -- I have on idea what these are,
    -- Use placeholder for now. Need to investigate what these actually are
    C.CUSTOM_INLINE _ _            -> parseParagraph contents
    C.CUSTOM_BLOCK _ _             -> parseParagraph contents
    C.THEMATIC_BREAK               -> [paragraph [noStyle [text "\n"]]]

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

-- | Convert CODE_BLOCK
toCodeBlock :: Text -> Text -> Block
toCodeBlock codeInfo code
    | T.null codeInfo = codeBlock "code" code
    | otherwise       = codeBlock codeInfo code

-- | Convert HEADING
toHeading :: Int -> [Node] -> Block
toHeading headingNum nodes =
    let level =
    -- Headers in scrapbox are opposite of what common markdowns are
          case headingNum of
              1 -> 4
              2 -> 3
              3 -> 2
              4 -> 1
              _ -> 1
    in heading level $ concatMap toSegments nodes

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
toBulletList nodes = bulletPoint 1 $ concatMap toBlocks nodes

-- | Apply 'LINEBREAK' between 'HEADING' section
--
-- [HEADING, b1, b2, b3, HEADING, b4, b5, b6, HEADING]
-- =>
-- [HEADING, b1, b2, b3, LINEBREAK, HEADING, b4, b5, b6, LINEBREAK, HEADING]
applyLinebreak :: [Block] -> [Block]
applyLinebreak []                               = []
applyLinebreak [b]                              = [b]
applyLinebreak (b:Types.HEADING hsize hcontent:rest) =
    b : Types.LINEBREAK : applyLinebreak (Types.HEADING hsize hcontent : rest)
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

    -- | Concatenate 'PARAGRAPH' blocks
    concatParagraph :: [Block] -> [Block]
    concatParagraph []  = []
    concatParagraph [n] = [n]
    concatParagraph (Types.PARAGRAPH stext1 : Types.PARAGRAPH stext2 : rest) =
        let concatedParagraph = Types.PARAGRAPH $ concatScrapText stext1 stext2
        in concatParagraph $ [concatedParagraph] <> rest
    concatParagraph (a : b : rest) = a : b : concatParagraph rest
