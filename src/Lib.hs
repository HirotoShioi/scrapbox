{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           RIO
import           RIO.List
import qualified RIO.Text       as T
import qualified Data.ByteString.Char8 as C8

import           Types

-- | Produce ScrapBox Markdown in human readable format
encodePretty :: Page -> ByteString
encodePretty = C8.unlines . encode

encode :: Page -> [ByteString]
encode page =
    let mds =  getContent $ pContent page
    in concatMap (map encodeUtf8 . encodeMarkdown) mds

encodeMarkdown :: Markdown -> [Text]
encodeMarkdown = \case
    BreakLine                      -> [""]
    BlockQuote text                -> [">" <> encodeText text]
    BulletPoints contents          -> encodeBulletPoints contents
    CodeBlock codeName code        -> encodeCodeBlock codeName code
    BulletLine text                -> ["\t" <> encodeText text]
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
    EndLine                             -> ""

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
                codeRest = map (\codeLine -> " " <> codeLine) rest
            [codeName] <> [codeHead'] <> codeRest

encodeTable :: TableContent -> [Text]
encodeTable tableContent = undefined

encodeBulletPoints :: [ScrapText] -> [Text]
encodeBulletPoints txt = map (\scrapText -> "\t" <> encodeText scrapText) txt

-- decode :: ???? -> Parser [Markdown]
-- decode = undefined
