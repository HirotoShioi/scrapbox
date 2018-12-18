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

toBlocks :: Node -> [Block]
toBlocks (Node _ nodeType contents) = case nodeType of
    PARAGRAPH                    -> [Paragraph $ ScrapText $ concatMap toContext contents, LineBreak]
    DOCUMENT                     -> concatMap toBlocks contents
    HEADING headerNum            -> [Header (HeaderSize 2) (concatMap toSegments contents)]
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

toSegments :: Node -> [Segment]
toSegments (Node _ nodeType contents) = case nodeType of
    TEXT textContent -> [SimpleText textContent]
    CODE codeContent -> [CodeNotation codeContent]
    LINK url title   -> [toLink contents url title]
    IMAGE url title  -> [toLink contents url title]
    _                -> concatMap toSegments contents -- Potentially cause infinite loop?

-- Need state monad to inherit style
toContext :: Node -> [Context]
toContext (Node _ nodeType contents) = case nodeType of
    EMPH             -> [Context Italic (concatMap toSegments contents)]
    STRONG           -> [Context Bold (concatMap toSegments contents)]
    TEXT textContent -> [Context NoStyle [SimpleText textContent]]
    CODE codeContent -> [Context NoStyle [CodeNotation codeContent]]
    LINK url title   -> [Context NoStyle [toLink contents url title]]
    IMAGE url title  -> [Context NoStyle [toLink contents url title]]
    _                -> concatMap toContext contents -- Potentially cause infinite loop?

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

extractTextFromNodes :: [Node] -> Text
extractTextFromNodes nodes = foldr
    (\(Node _ nodeType nodes') acc -> extractText nodeType <> extractTextFromNodes nodes' <> acc) mempty nodes
  where
    extractText :: NodeType -> Text
    extractText = \case
        TEXT text -> text
        CODE code -> code
        _         -> mempty -- Just throw an error?