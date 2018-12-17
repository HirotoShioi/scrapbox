{-# LANGUAGE OverloadedStrings #-}

module CommonMark where

import RIO

import CMark
import Types
import qualified RIO.Text as T

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
    CODE_BLOCK codeInfo code     -> 
        let codeName = if T.null codeInfo then "code" else codeInfo
        in [CodeBlock (CodeName codeName) (CodeSnippet (T.lines code))]
    LIST _                       -> [BulletList $ map toScrapText contents]
    ITEM                         -> concatMap toBlocks contents
    SOFTBREAK                    -> [LineBreak]
    LINEBREAK                    -> [LineBreak]
    LINK url title               -> [Paragraph $ ScrapText [Context NoStyle [Link (Just title) (Url url)]]]
    HTML_BLOCK htmlContent       -> [CodeBlock (CodeName "html") (CodeSnippet (T.lines htmlContent))]
    IMAGE url _                  -> [Thumbnail (Url url)]
    HTML_INLINE htmlContent      -> [CodeBlock (CodeName "html") (CodeSnippet (T.lines htmlContent))]
    BLOCK_QUOTE                  -> [BlockQuote $ ScrapText $ concatMap toContext contents]
    CUSTOM_INLINE onEnter onExit -> undefined -- ??
    CUSTOM_BLOCK onEnter onExit  -> undefined -- ??
    THEMATIC_BREAK               -> undefined -- ??

toSegments :: Node -> [Segment]
toSegments (Node _ nodeType contents) = case nodeType of
    TEXT textContent -> [SimpleText textContent]
    CODE codeContent -> [CodeNotation codeContent]
    LINK url title   -> [Link (Just title) (Url url)]
    _                -> concatMap toSegments contents

toContext :: Node -> [Context]
toContext (Node _ nodeType contents) = case nodeType of
    EMPH             -> [Context Italic (concatMap toSegments contents)]
    STRONG           -> [Context Bold (concatMap toSegments contents)]
    TEXT textContent -> [Context NoStyle [SimpleText textContent]]
    CODE codeContent -> [Context NoStyle [CodeNotation codeContent]]
    LINK url title   -> [toLink contents title url]
    _                -> concatMap toContext contents
  where
    toLink :: [Node] -> Title -> CMark.Url -> Context
    toLink [] title url     = Context NoStyle [Link (Just title) (Url url)]
    toLink ((Node _ nodeType _):cs) title url = case nodeType of
        TEXT textContent -> Context NoStyle [Link (Just textContent) (Url url)]
        CODE codeContent -> Context NoStyle [Link (Just codeContent) (Url url)]
        others           -> Context NoStyle [Link (Just title) (Url url)]

toScrapText :: Node -> ScrapText
toScrapText node = ScrapText $ toContext node