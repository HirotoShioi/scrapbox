{-| Module for 'Block' parser
-}

{-# LANGUAGE OverloadedStrings #-}

module Parser.Scrapbox
    ( runScrapboxParser
    ) where

import           RIO                           hiding (many, try, (<|>))
import qualified RIO.Text                      as T

import           Network.URI                   (isURI)
import           Text.ParserCombinators.Parsec (ParseError, Parser, anyChar,
                                                between, char, eof, many, many1,
                                                manyTill, noneOf, oneOf, parse,
                                                sepBy1, space, string, try,
                                                unexpected, (<|>))

import           Types                         (Block (..), BulletSize (..),
                                                CodeName (..), CodeSnippet (..),
                                                HeaderSize (..), Markdown (..),
                                                TableContent (..),
                                                TableName (..), Url (..))

import           Parser.Inline                 (runInlineParserM)
import           Parser.Text                   (runScrapTextParserM)


--------------------------------------------------------------------------------
-- Block parser
--------------------------------------------------------------------------------

-- | Parser for 'LineBreak'
lineBreakParser :: Parser Block
lineBreakParser = do
    _ <- endOfLine
    return LineBreak

-- | Parser for 'Paragraph'
paragraphParser :: Parser Block
paragraphParser = do
    str       <- getString
    scrapText <- runScrapTextParserM str
    return $ Paragraph scrapText

-- | Parser for 'Thumbnail'
thumbnailParser :: Parser Block
thumbnailParser = do
    thumbnailLink <- between (char '[') (char ']') (many1 $ noneOf "]")
    _             <- endOfLine
    if isURI thumbnailLink
        then return $ Thumbnail (Url $ fromString thumbnailLink)
        else unexpected "Cannot parse as Thumbnail since the content is not URI"

-- | Parser for 'BlockQuote'
blockQuoteParser :: Parser Block
blockQuoteParser = do
    _         <- char '>'
    str       <- getString
    scrapText <- runScrapTextParserM str
    return $ BlockQuote scrapText

-- | Parser for 'Header'
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

-- | Parser for 'BulletPoint'
bulletPointParser :: Parser Block
bulletPointParser = do
    symbolLen <- length <$> many1 space
    str       <- getString
    scrapText <- runScrapTextParserM str
    return $ BulletPoint (BulletSize symbolLen) scrapText

-- | Parser for 'CodeBlock'
codeBlockParser :: Parser Block
codeBlockParser = do
    _        <- string "code:"
    codeName <- manyTill anyChar endOfLine
    snippet  <- T.unlines <$> many codeSnippetParser
    return $ CodeBlock (CodeName $ fromString codeName) (CodeSnippet snippet)
  where
    codeSnippetParser :: Parser Text
    codeSnippetParser = do
        _        <- oneOf "\t "
        codeLine <- manyTill anyChar endOfLine
        return $ fromString codeLine

-- | Parser to 'Table'
tableParser :: Parser Block
tableParser = do
    _            <- string "table:"
    tableName    <- manyTill anyChar endOfLine
    tableContent <- manyTill rowParser endOfLine
    return $ Table (TableName $ fromString tableName) (TableContent tableContent)
  where
    rowParser :: Parser [Text]
    rowParser = do
        _    <- char '\t' <|> space
        row  <- sepBy1 (many $ noneOf "\t\n") (try $ char '\t')
        _    <- endOfLine
        return $ map fromString row

--------------------------------------------------------------------------------
-- Markdown parser
--------------------------------------------------------------------------------

-- | Parser for 'Markdown'
markdownParser :: Parser Markdown
markdownParser = Markdown <$> manyTill blockParser eof
  where
    blockParser :: Parser Block
    blockParser =
            try lineBreakParser
        <|> try headerParser
        <|> try thumbnailParser
        <|> try blockQuoteParser
        <|> try bulletPointParser
        <|> try codeBlockParser
        <|> try tableParser
        <|> try paragraphParser

-- | Run scrapbox parser on given 'String'
runScrapboxParser :: String -> Either ParseError Markdown
runScrapboxParser = parse markdownParser "Scrapbox parser"

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- | End of line parser
endOfLine :: Parser ()
endOfLine = void (char '\n') <|> void (string "\r\n") <|> eof

-- | Consume string until end of line
getString :: Parser String
getString = manyTill anyChar (try endOfLine)
