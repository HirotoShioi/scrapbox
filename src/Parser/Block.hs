{-| Module for 'Block' parser
-}

{-# LANGUAGE OverloadedStrings #-}

module Parser.Block where

import           RIO                           hiding (many, try, (<|>))
import qualified RIO.Text                      as T

import           Network.URI                   (isURI)
import           Text.ParserCombinators.Parsec

import           Types                         (Block (..), BulletSize (..),
                                                CodeName (..), CodeSnippet (..),
                                                HeaderSize (..), Markdown (..),
                                                Url (..))

import           Parser.Inline                 (runInlineParserM)
import           Parser.Text                   (runScrapTextParserM)


--------------------------------------------------------------------------------
-- Block parser
--------------------------------------------------------------------------------

lineBreakParser :: Parser Block
lineBreakParser = do
    _ <- endOfLine
    return LineBreak

paragraphParser :: Parser Block
paragraphParser = do
    str       <- getString
    scrapText <- runScrapTextParserM str
    return $ Paragraph scrapText

thumbnailParser :: Parser Block
thumbnailParser = do
    thumbnailLink <- between (char '[') (char ']') (many1 $ noneOf "]")
    _             <- endOfLine
    if isURI thumbnailLink
        then return $ Thumbnail (Url $ fromString thumbnailLink)
        else unexpected "Cannot parse as Thumbnail since the content is not URI"

blockQuoteParser :: Parser Block
blockQuoteParser = do
    _         <- char '>'
    str       <- getString
    scrapText <- runScrapTextParserM str
    return $ BlockQuote scrapText

headerParser :: Parser Block
headerParser = do
    _         <- char '['
    symbolLen <- length <$> many1 (char '*')
    _         <- space
    str       <- many (noneOf "]")
    _         <- char ']'
    _         <- endOfLine
    segments  <- runInlineParserM str
    return $ Header (HeaderSize symbolLen) segments

bulletPointParser :: Parser Block
bulletPointParser = do
    symbolLen <- length <$> many1 space
    str       <- getString
    scrapText <- runScrapTextParserM str
    return $ BulletPoint (BulletSize symbolLen) scrapText

codeBlockParser :: Parser Block
codeBlockParser = do
    _        <- string "code:"
    codeName <- manyTill anyChar endOfLine
    snippet  <- T.unlines <$> many codeSnippetParser
    return $ CodeBlock (CodeName $ fromString codeName) (CodeSnippet snippet)
  where
    codeSnippetParser :: Parser Text
    codeSnippetParser = do
        _        <- string "\t" <|> string " "
        codeLine <- manyTill anyChar endOfLine
        return $ fromString codeLine

--------------------------------------------------------------------------------
-- Markdown parser
--------------------------------------------------------------------------------

markdownParser :: Parser Markdown
markdownParser = Markdown <$> manyTill blockParser eof
  where
    blockParser :: Parser Block
    blockParser =
            try thumbnailParser
        <|> try blockQuoteParser
        <|> try headerParser
        <|> try bulletPointParser
        <|> try lineBreakParser
        <|> try codeBlockParser
        <|> try paragraphParser

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

endOfLine :: Parser ()
endOfLine = void (char '\n') <|> void (string "\r\n") <|> eof

getString :: Parser String
getString = manyTill anyChar (try endOfLine)
