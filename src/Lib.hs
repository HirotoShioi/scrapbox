{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib 
    ( encodePretty
    , encodeMarkdown
    , mkMarkdown
    ) where

import           RIO
import qualified RIO.Text              as T
import           Types

-- Pretty print Mark down
encodePretty :: Markdown -> Text
encodePretty (Markdown blocks) = T.unlines $ concatMap encodeBlock blocks

-- | Encode given `Markdown' into list of ByteStrings
encodeMarkdown :: Markdown -> [ByteString]
encodeMarkdown (Markdown blocks) =
    concatMap (map encodeUtf8 . encodeBlock) blocks

-- | Smart constructor for creating 'Markdown' with given '[Block]'
mkMarkdown :: [Block] -> Markdown
mkMarkdown = Markdown

-- Encode blocks
encodeBlock :: Block -> [Text]
encodeBlock = \case
    BreakLine                             -> [""]
    BlockQuote scrapText                  -> [">" <> encodeText scrapText]
    BulletPoints contents                 -> encodeBulletPoints contents
    BulletLine (BulletSize num) scrapText -> [T.replicate num " " <> encodeText scrapText]
    CodeBlock codeName code               -> encodeCodeBlock codeName code
    Document scrapText                    -> [encodeText scrapText]
    Header (HeaderSize num) ctx           -> [encodeHeader num ctx]
    Table table                           -> encodeTable table
    Thumbnail (Url url)                   -> [blocked url]

-- | Encode given 'ScrapText' into text
encodeText :: ScrapText -> Text
encodeText (ScrapText ctxs) = foldr (\scrap acc-> encodeScrapText scrap <> acc) mempty ctxs

-- | Encode given 'Scrap' into text
encodeScrapText :: Context -> Text
encodeScrapText (Context style content) = encodeWithStyle style content

encodeContent :: Content -> Text
encodeContent ctxs = foldr (\ctx acc -> encodeScrapContext ctx <> acc) mempty ctxs
  where
    encodeScrapContext :: Segment -> Text
    encodeScrapContext = \case
        CodeNotation code              -> "`" <> code <> "`"
        Link (Just linkName) (Url url) -> blocked $ url <> " " <> linkName
        Link Nothing (Url url)         -> blocked url
        SimpleText text                 -> text

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
encodeBulletPoints text = map (\scrapText -> "\t" <> encodeText scrapText) text

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
    let italic         = if isItalic then "/" else mempty
        strikeThrough  = if isStrikeThrough then "-" else mempty
        bold           = if isBold then "*" else mempty
        headerNum'     = if isBold then 0 else headerNum
        combinedSyntax = mconcat
            [ T.replicate headerNum' "*"
            , bold
            , italic
            , strikeThrough
            , " "
            ]
    in blocked $ combinedSyntax <> (encodeContent content)

-- | Encode header
encodeHeader :: Int -> Content -> Text
encodeHeader headerSize content =
    let style = StyleData headerSize False False False
    in encodeCustomStyle style content
