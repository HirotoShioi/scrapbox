{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           RIO
import           RIO.List
import qualified RIO.Text       as T
import qualified Data.ByteString.Char8 as C8

import           Types

-- | Produce ScrapBox Markdown in human readable format
encodePretty :: Markdown -> ByteString
encodePretty = C8.unlines . encode

encode :: Markdown -> [ByteString]
encode md =
    let blocks =  getContent md
    in concatMap (map encodeUtf8 . encodeMarkdown) blocks

encodeMarkdown :: Block -> [Text]
encodeMarkdown = \case
    BreakLine                      -> [""]
    BlockQuote text                -> [">" <> encodeText text]
    BulletPoints contents          -> encodeBulletPoints contents
    BulletLine text                -> ["\t" <> encodeText text]
    CodeBlock codeName code        -> encodeCodeBlock codeName code
    Header headerSize text         -> [encodeHeader headerSize (encodeText text)]
    Simple text                    -> [encodeText text]
    Table table                    -> encodeTable table
    Thumbnail (Url url)            -> [block url]

encodeText :: ScrapText -> Text
encodeText (ScrapText scraps) = foldr (\scrap acc-> encodeScrapText scrap <> acc) mempty scraps

encodeScrapText :: Scrap -> Text
encodeScrapText (Scrap style scrap) =
    let encodedScrap = case scrap of
            CodeNotation code              -> "`" <> code <> "`"
            Link (Just linkName) (Url url) -> block $ linkName <> " " <> url
            Link Nothing (Url url)         -> block url
            PlainText text                 -> text
    in encodeWithStyle style encodedScrap

encodeWithStyle :: Style -> Text -> Text
encodeWithStyle style text = case style of
    None -> text
    Bold -> 
        let boldStyle = StyleData 0 True False False
        in encodeStyled boldStyle text
    Italic -> 
        let italicStyle = StyleData 0 False True False
        in encodeStyled italicStyle text
    StrikeThrough ->
        let strikeThroughStyle = StyleData 0 False False True
        in encodeStyled strikeThroughStyle text
    Custom style' -> encodeStyled style' text

encodeStyled :: StyleData -> Text -> Text
encodeStyled (StyleData headerNum isBold isItalic isStrikeThrough) text =
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
    in block $ combinedSyntax <> text

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
encodeBulletPoints text = map (\scrapText -> "\t" <> encodeText scrapText) text

encodeHeader :: Int -> Text -> Text
encodeHeader headerSize scrapText = 
    let style     = StyleData headerSize False False False
    in encodeStyled style scrapText

block :: Text -> Text
block content = "[" <> content <> "]"

-- decodeHtml :: ???? -> Parser Content
-- decodeHtml = undefined

-- decodeMarkdown :: ???? -> Parser Content
-- decodeMarkdown = undefined

-- Need to make an chart