{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import RIO
import RIO.List
import qualified RIO.Text as T
import qualified RIO.ByteString as BS

import Types

-- | Produce ScrapBox Markdown in human readable format
encodePretty :: Page -> Text
encodePretty page =
    let mds =  getContent $ pContent page
        bss = map (\md -> encodeMarkdown md) mds
    in T.unlines $ concat bss

encodeMarkdown :: Markdown -> [Text]
encodeMarkdown = \case
    BreakLine                      -> [""]
    BlockQuote text                -> ["> " <> encodeText text]
    BulletPoints contents          -> encodeBulletPoints contents
    CodeBlock codeName code        -> encodeCodeBlock codeName code
    NewLine text                   -> ["\t" <> encodeText text]
    Simple text                    -> [encodeText text]
    Table table                    -> encodeTable table
    Thumbnail (Url url)            -> [block url]

encodeText :: ScrapText -> Text
encodeText = \case
    CodeNotation code next              -> "`" <> code <> "`" <> encodeText next
    Link (Just linkName) (Url url) next -> block (linkName <> " " <> url) <> encodeText next
    Link Nothing (Url url) next         -> block url <> encodeText next
    SimpleText text next                -> text <> encodeText next
    Styled style text next              -> encodeStyled style text <> encodeText next
    End                                 -> ""

block :: Text -> Text
block content = "[" <> content <> "]"

encodeStyled :: Style -> ScrapText -> Text
encodeStyled (Style headerNum isBold isItalic isStrikeThrough) md =
    let italic         = if isItalic then "/" else ""
        strikeThrough  = if isStrikeThrough then "-" else ""
        bold           = if isBold then "*" else ""
        headerNum'     = if isBold then 0 else headerNum
        combinedSyntax = mconcat 
            [ T.replicate headerNum' "*"
            , bold
            , italic
            , strikeThrough
            , " "
            ]
    in block $ combinedSyntax <> encodeText md

encodeCodeBlock :: CodeName -> Text -> [Text]
encodeCodeBlock (CodeName name) code = do
    let codeName = "code:" <> name
    case headMaybe (T.lines code) of
        Nothing -> [codeName]
        Just codeHead -> do
            let codeHead' = "\t" <> codeHead
                rest      = snd $ splitAt 1 (T.lines code)
                restCoded = map (\codeLine -> " " <> codeLine) rest
            [codeName] <> [codeHead'] <> restCoded

encodeTable :: TableContent -> [Text]
encodeTable tableContent = undefined

encodeBulletPoints :: [ScrapText] -> [Text]
encodeBulletPoints txt = map (\scrapText -> " " <> encodeText scrapText) txt


-- decode :: ???? -> Parser [Markdown]
-- decode = undefined