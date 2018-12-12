{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           RIO
import           RIO.List
import qualified RIO.Text       as T
import qualified Data.ByteString.Char8 as C8

import           Types

-- | Produce ScrapBox Markdown in human readable format
-- Not working well
encodePretty :: Markdown -> ByteString
encodePretty = C8.unlines . encodeMarkdown

-- | Encode given `Markdown' into list of ByteStrings
encodeMarkdown :: Markdown -> [ByteString]
encodeMarkdown md =
    let blocks =  getMarkdown md
    in concatMap (map encodeUtf8 . encodeBlocks) blocks

-- Encode blocks
encodeBlocks :: Block -> [Text]
encodeBlocks = \case
    BreakLine               -> [""]
    BlockQuote text         -> [">" <> encodeText text]
    BulletPoints contents   -> encodeBulletPoints contents
    BulletLine text         -> ["\t" <> encodeText text]
    CodeBlock codeName code -> encodeCodeBlock codeName code
    Header headerSize ctx   -> [encodeHeader headerSize ctx]
    Simple text             -> [encodeText text]
    Table table             -> encodeTable table
    Thumbnail (Url url)     -> [blocked url]

-- | Encode given 'ScrapText' into text
encodeText :: ScrapText -> Text
encodeText (ScrapText scraps) = foldr (\scrap acc-> encodeScrapText scrap <> acc) mempty scraps

-- | Encode given 'Scrap' into text
encodeScrapText :: Scrap -> Text
encodeScrapText (Scrap style ctx) = encodeWithStyle style ctx

encodeContext :: Context -> Text
encodeContext ctxs = foldr (\ctx acc -> encodeScrapContext ctx <> acc) mempty ctxs
  where
    encodeScrapContext :: ScrapContext -> Text
    encodeScrapContext = \case
        CodeNotation code              -> "`" <> code <> "`"
        Link (Just linkName) (Url url) -> blocked $ linkName <> " " <> url
        Link Nothing (Url url)         -> blocked url
        PlainText text                 -> text

-- | Encode with style (Do not export this)
encodeWithStyle :: Style -> Context -> Text
encodeWithStyle style ctx = case style of
    None -> encodeContext ctx
    Bold -> 
        let boldStyle = StyleData 0 True False False
        in encodeStyled boldStyle ctx
    Italic -> 
        let italicStyle = StyleData 0 False True False
        in encodeStyled italicStyle ctx
    StrikeThrough ->
        let strikeThroughStyle = StyleData 0 False False True
        in encodeStyled strikeThroughStyle ctx
    Custom style' -> encodeStyled style' ctx

encodeStyled :: StyleData -> Context -> Text
encodeStyled (StyleData headerNum isBold isItalic isStrikeThrough) ctx =
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
    in blocked $ combinedSyntax <> (encodeContext ctx)

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

-- | Encode header
encodeHeader :: Int -> Context -> Text
encodeHeader headerSize ctx = 
    let style     = StyleData headerSize False False False
    in encodeStyled style ctx

-- | Add an block to a given encoded text
blocked :: Text -> Text
blocked content = "[" <> content <> "]"