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
    PARAGRAPH                    -> [Paragraph $ ScrapText $ map toContext contents]
    DOCUMENT                     -> concatMap toBlocks contents
    HEADING headerNum            -> [Header (HeaderSize 2) (concatMap toSegments contents)]
    EMPH                         -> [Paragraph $ ScrapText [Context Italic (concatMap toSegments contents)]]
    STRONG                       -> [Paragraph $ ScrapText [Context Bold (concatMap toSegments contents)]]
    TEXT textContent             -> [Paragraph $ ScrapText [Context NoStyle [SimpleText textContent]]]
    CODE codeContent             -> [Paragraph $ ScrapText [Context NoStyle [CodeNotation codeContent]]]
    CODE_BLOCK codeInfo code     -> [CodeBlock (CodeName codeInfo) (CodeSnippet (T.lines code))]
    LIST _                       -> [BulletList $ ]
    THEMATIC_BREAK               -> undefined
    ITEM                         -> concatMap toBlocks contents
    SOFTBREAK                    -> [LineBreak]
    LINEBREAK                    -> [LineBreak]
    LINK url title               -> [Paragraph $ ScrapText [Context NoStyle [Link (Just title) (Url url)]]]
    HTML_BLOCK htmlContent       -> [CodeBlock (CodeName "html") (CodeSnippet (T.lines htmlContent))]
    CUSTOM_BLOCK onEnter onExit  -> undefined
    IMAGE url title              -> undefined 
    HTML_INLINE htmlContent      -> [CodeBlock (CodeName "html") (CodeSnippet (T.lines htmlContent))]
    BLOCK_QUOTE                  -> undefined
    CUSTOM_INLINE onEnter onExit -> undefined

toSegments :: Node -> [Segment]
toSegments (Node _ nodeType contents) = case nodeType of
    TEXT textContent -> [SimpleText textContent]
    CODE codeContent -> [CodeNotation codeContent]
    _                -> concatMap toSegments contents

toContext :: Node -> Context
toContext (Node _ nodeType contents) = case nodeType of
    EMPH             -> Context Italic (concatMap toSegments contents)
    STRONG           -> Context Bold (concatMap toSegments contents)
    _                -> Context NoStyle (concatMap toSegments contents)

toScrapText :: Node -> ScrapText
to