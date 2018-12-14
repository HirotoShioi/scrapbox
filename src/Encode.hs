{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Encode
    ( encodePretty
    , encodeMarkdown
    ) where

import           RIO
import qualified RIO.Text as T

import           Types    (Block (..), BulletSize (..), CodeName (..),
                           CodeSnippet (..), Content, Context (..),
                           HeaderSize (..), Markdown (..), ScrapText (..),
                           Segment (..), Style (..), StyleData (..),
                           TableContent, Url (..))

-- Pretty print Mark down
encodePretty :: Markdown -> Text
encodePretty (Markdown blocks) = T.unlines $ concatMap encodeBlock blocks

-- | Encode given `Markdown' into list of ByteStrings
encodeMarkdown :: Markdown -> [ByteString]
encodeMarkdown (Markdown blocks) =
    concatMap (map encodeUtf8 . encodeBlock) blocks

-- Encode blocks
encodeBlock :: Block -> [Text]
encodeBlock = \case
    LineBreak                          -> [""]
    BlockQuote stext                   -> [">" <> encodeText stext]
    BulletList contents                -> encodeBulletPoints contents
    BulletPoint (BulletSize num) stext -> [T.replicate num " " <> encodeText stext]
    CodeBlock codeName code            -> encodeCodeBlock codeName code
    Document stext                     -> [encodeText stext]
    Header (HeaderSize num) contents   -> [encodeHeader num contents]
    Table tableContent                 -> encodeTable tableContent
    Thumbnail (Url url)                -> [blocked url]

-- | Encode given 'ScrapText' into text
encodeText :: ScrapText -> Text
encodeText (ScrapText ctxs) = foldr (\scrap acc-> encodeScrapText scrap <> acc) mempty ctxs

-- | Encode given 'Scrap' into text
encodeScrapText :: Context -> Text
encodeScrapText (Context style content) = encodeWithStyle style content

encodeContent :: Content -> Text
encodeContent ctxs = foldr (\ctx acc -> encodeSegment ctx <> acc) mempty ctxs
  where
    encodeSegment :: Segment -> Text
    encodeSegment = \case
        CodeNotation code              -> "`" <> code <> "`"
        HashTag text                   -> "#" <> text
        Link (Just linkName) (Url url) -> blocked $ url <> " " <> linkName
        Link Nothing (Url url)         -> blocked url
        SimpleText text                -> text

-- | Encode 'CodeBlock'
encodeCodeBlock :: CodeName -> CodeSnippet -> [Text]
encodeCodeBlock (CodeName name) code = do
    let codeName = "code:" <> name
    let codeContent = map (\line -> " " <> line) (T.lines $ getCodeSnippet code)
    [codeName] <> codeContent

-- | Encode an table
encodeTable :: TableContent -> [Text]
encodeTable tableContent = undefined

-- | Encode bulletpoints
encodeBulletPoints :: [ScrapText] -> [Text]
encodeBulletPoints stexts = map (\stext -> "\t" <> encodeText stext) stexts

-- | Add an block to a given encoded text
blocked :: Text -> Text
blocked content = "[" <> content <> "]"

-- | Encode with style (Do not export this)
encodeWithStyle :: Style -> Content -> Text
encodeWithStyle style ctx = case style of
    NoStyle -> encodeContent ctx
    Bold ->
        let boldStyle = StyleData 0 True False False
        in encodeCustomStyle boldStyle ctx
    Italic ->
        let italicStyle = StyleData 0 False True False
        in encodeCustomStyle italicStyle ctx
    StrikeThrough ->
        let strikeThroughStyle = StyleData 0 False False True
        in encodeCustomStyle strikeThroughStyle ctx
    CustomStyle customStyle -> encodeCustomStyle customStyle ctx

encodeCustomStyle :: StyleData -> Content -> Text
encodeCustomStyle (StyleData headerNum isBold isItalic isStrikeThrough) content =
    let italicSymbol         = if isItalic then "/" else mempty
        strikeThroughSymbol  = if isStrikeThrough then "-" else mempty
        boldSymbol           = if isBold then "*" else mempty
        headerNum'           = if isBold then 0 else headerNum
        combinedSyntax       = mconcat
            [ T.replicate headerNum' "*"
            , boldSymbol
            , italicSymbol
            , strikeThroughSymbol
            , " "
            ]
    in blocked $ combinedSyntax <> (encodeContent content)

-- | Encode header
encodeHeader :: Int -> Content -> Text
encodeHeader headerSize content =
    let style = StyleData headerSize False False False
    in encodeCustomStyle style content
