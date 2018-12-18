{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module CommonMark where

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

--- It would be nice if these functions are wrapped in Either like this:
--  newtype Parser a = Parser (Either ParserException a)

-- | Convert 'Node' into list of 'Block'
-- Still unfinished
toBlocks :: Node -> [Block]
toBlocks (Node _ nodeType contents) = case nodeType of
    PARAGRAPH                    -> [Paragraph $ ScrapText $ concatMap toContext contents, LineBreak]
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
    LINK url title               -> [Paragraph $ ScrapText [Context NoStyle [Link (Just title) (Url url)]]]
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

-- | Convert 'Node' into list of 'Context'
-- Need state monad to inherit style from parent node
toContext :: Node -> [Context]
toContext (Node _ nodeType contents) = case nodeType of
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
        _         -> mempty -- Just throw an error?