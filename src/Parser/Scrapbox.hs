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
                                                between, char, eof, lookAhead,
                                                many, many1, manyTill, noneOf,
                                                oneOf, parse, sepBy1, space,
                                                string, try, unexpected, (<|>))

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
bulletPointParser :: Int -> Parser Block
bulletPointParser indentNum = do
    -- Look ahead and count the number of spaces
    numOfIndents <- length <$> lookAhead (try $ many1 $ oneOf indent)
    when (numOfIndents <= indentNum) $ fail "less indent"
    blocks      <- many1 $ blockParser numOfIndents
    let bulletSize = numOfIndents - indentNum
    return $ BulletPoint (BulletSize bulletSize) blocks

-- | Parser for 'CodeBlock'
codeBlockParser :: Int -> Parser Block
codeBlockParser indentNum = do
    _        <- string "code:"
    codeName <- manyTill anyChar endOfLine
    snippet  <- T.unlines <$> many codeSnippetParser
    return $ CodeBlock (CodeName $ fromString codeName) (CodeSnippet snippet)
  where
    codeSnippetParser :: Parser Text
    codeSnippetParser = do
        consumeIndent indentNum
        _        <- oneOf indent
        codeLine <- manyTill anyChar endOfLine
        return $ fromString codeLine

-- | Parser to 'Table'
tableParser :: Int -> Parser Block
tableParser indentNum = do
    _            <- string "table:"
    tableName    <- manyTill anyChar endOfLine
    tableContent <- manyTill rowParser endOfLine
    return $ Table (TableName $ fromString tableName) (TableContent tableContent)
  where
    rowParser :: Parser [Text]
    rowParser = do
        consumeIndent indentNum
        _    <- oneOf indent
        row  <- sepBy1 (many $ noneOf "\t\n") (try $ char '\t')
        _    <- endOfLine
        return $ map fromString row


blockParser :: Int
            -- ^ Number of indents
            -> Parser Block
blockParser indentNum =
        try (consumeIndent indentNum *> lineBreakParser)
    <|> try (consumeIndent indentNum *> headerParser)
    <|> try (consumeIndent indentNum *> thumbnailParser)
    <|> try (consumeIndent indentNum *> blockQuoteParser)
    <|> try (bulletPointParser indentNum)
    <|> try (consumeIndent indentNum *> codeBlockParser indentNum)
    <|> try (consumeIndent indentNum *> tableParser indentNum)
    <|> try (consumeIndent indentNum *> paragraphParser)

--------------------------------------------------------------------------------
-- Markdown parser
--------------------------------------------------------------------------------

-- | Parser for 'Markdown'
markdownParser :: Parser Markdown
markdownParser = Markdown <$> manyTill (blockParser 0) eof

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

consumeIndent :: Int -> Parser ()
consumeIndent indentNum = replicateM_ indentNum (oneOf indent)

indent :: String
indent = " \t"