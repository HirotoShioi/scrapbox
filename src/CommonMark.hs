{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module CommonMark 
    ( test
    , testHeader
    , toBlocks
    ) where

import           RIO

import           CMark
import qualified RIO.Text as T
import           Types

test :: IO Node
test = do
    markDown <- readFileUtf8 "./docs/example.md"
    let options = [optSafe, optHardBreaks]
    let parsed = commonmarkToNode options markDown
    return parsed

testHeader :: IO Node
testHeader = do
    markDown <- readFileUtf8 "./docs/headers.md"
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
    EMPH                         -> [Paragraph $ ScrapText [Context Italic (concatMap toSegments contents)]]
    STRONG                       -> [Paragraph $ ScrapText [Context Bold (concatMap toSegments contents)]]
    TEXT textContent             -> [Paragraph $ ScrapText [Context NoStyle [SimpleText textContent]]]
    CODE codeContent             -> [Paragraph $ ScrapText [Context NoStyle [CodeNotation codeContent]]]
    CODE_BLOCK codeInfo code     -> [toCodeBlock codeInfo code]
    LIST _                       -> [BulletList $ map toScrapText contents]
    ITEM                         -> concatMap toBlocks contents
    SOFTBREAK                    -> [LineBreak]
    LINEBREAK                    -> [LineBreak]
    LINK url title               -> [Paragraph $ ScrapText [Context NoStyle [toLink contents url title]]]
    HTML_BLOCK htmlContent       -> [CodeBlock (CodeName "html") (CodeSnippet htmlContent)]
    IMAGE url _                  -> [Thumbnail (Url url)]
    HTML_INLINE htmlContent      -> [CodeBlock (CodeName "html") (CodeSnippet htmlContent)]
    BLOCK_QUOTE                  -> [BlockQuote $ ScrapText $ concatMap toContext contents]
    CUSTOM_INLINE onEnter onExit -> undefined -- ??
    CUSTOM_BLOCK onEnter onExit  -> undefined -- ??
    THEMATIC_BREAK               -> undefined -- ??

-- | Convert 'Node' into list of 'Segment'
toSegments :: Node -> [Segment]
toSegments (Node _ nodeType contents) = case nodeType of
    TEXT textContent -> [SimpleText textContent]
    CODE codeContent -> [CodeNotation codeContent]
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
        let concatedParagraph = Paragraph $ concatSText stext1 stext2
        in concatParagraph $ [concatedParagraph] <> rest
    concatParagraph (a:b:rest) = a : b : concatParagraph rest

    -- Concatenate 'ScrapText'
    concatSText :: ScrapText -> ScrapText -> ScrapText
    concatSText (ScrapText ctx1) (ScrapText ctx2) = (ScrapText $ concatContext $ ctx1 <> ctx2)

    -- Add breaks after Paragraph block
    -- Perhaps add this as an option when rendering?
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
        EMPH             -> [Context Italic (concatMap toSegments contents)]
        STRONG           -> [Context Bold (concatMap toSegments contents)]
        TEXT textContent -> [Context NoStyle [SimpleText textContent]]
        CODE codeContent -> [Context NoStyle [CodeNotation codeContent]]
        LINK url title   -> [Context NoStyle [toLink contents url title]]
        IMAGE url title  -> [Context NoStyle [toLink contents url title]]
        -- Potentially cause infinite loop?
        _                -> concatMap toContext contents

toScrapText :: Node -> ScrapText
toScrapText node = ScrapText $ toContext node

toLink :: [Node] -> CMark.Url -> Title -> Segment
toLink nodes url title
    | T.null title = mkLink url (extractTextFromNodes nodes)
    | null nodes   = mkLink url title
    | otherwise    = mkLink url title
  where
    mkLink :: CMark.Url -> Text -> Segment
    mkLink url' title'
        | T.null title' = Link Nothing (Url url')
        | otherwise     = Link (Just title') (Url url')

toCodeBlock :: Text -> Text -> Block
toCodeBlock codeInfo code
    | T.null codeInfo = CodeBlock (CodeName "code") (CodeSnippet code)
    | otherwise       = CodeBlock (CodeName codeInfo) (CodeSnippet code)

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

extractTextFromNodes :: [Node] -> Text
extractTextFromNodes nodes = foldr
    (\(Node _ nodeType nodes') acc 
        -> extractText nodeType <> extractTextFromNodes nodes' <> acc
    ) mempty nodes
  where
    extractText :: NodeType -> Text
    extractText = \case
        TEXT text -> text
        CODE code -> code
        -- For now, we're going to ignore everything else
        _         -> mempty