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
import           Utils                         (whenJust)

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
bulletPointParser :: Maybe Int -> Parser Block
bulletPointParser mNumOfSpaces = do
    consumeSpace mNumOfSpaces
    -- Look ahead and count the number of spaces
    numOfSpaces <- length <$> lookAhead (try $ many1 $ oneOf " \t")
    blocks      <- many1 $ do
        _ <- replicateM_ numOfSpaces (oneOf " \t")
        let s = fromMaybe 0 mNumOfSpaces
        blockParser (Just $ numOfSpaces + s)
    return $ BulletPoint (BulletSize numOfSpaces) blocks

-- | Parser for 'CodeBlock'
codeBlockParser :: Maybe Int -> Parser Block
codeBlockParser mNumberOfSpaces = do
    consumeSpace mNumberOfSpaces
    _        <- string "code:"
    codeName <- manyTill anyChar endOfLine
    snippet  <- T.unlines <$> many (codeSnippetParser mNumberOfSpaces)
    return $ CodeBlock (CodeName $ fromString codeName) (CodeSnippet snippet)
  where
    codeSnippetParser :: Maybe Int -> Parser Text
    codeSnippetParser mSpaces = do
        consumeSpace mSpaces
        _        <- oneOf "\t "
        codeLine <- manyTill anyChar endOfLine
        return $ fromString codeLine

-- | Parser to 'Table'
tableParser :: Maybe Int -> Parser Block
tableParser mNumberOfSpaces = do
    _            <- string "table:"
    tableName    <- manyTill anyChar endOfLine
    tableContent <- manyTill (rowParser mNumberOfSpaces) endOfLine
    return $ Table (TableName $ fromString tableName) (TableContent tableContent)
  where
    rowParser :: Maybe Int -> Parser [Text]
    rowParser mSpaces = do
        consumeSpace mSpaces
        _    <- char '\t' <|> space
        row  <- sepBy1 (many $ noneOf "\t\n") (try $ char '\t')
        _    <- endOfLine
        return $ map fromString row


blockParser :: Maybe Int
            -- ^ Number of spaces for bullet point
            -> Parser Block
blockParser mNumOfSpaces =
        try lineBreakParser
    <|> try headerParser
    <|> try thumbnailParser
    <|> try blockQuoteParser
    <|> try (bulletPointParser mNumOfSpaces)
    <|> try (codeBlockParser mNumOfSpaces)
    <|> try (tableParser mNumOfSpaces)
    <|> try paragraphParser

consumeSpace :: Maybe Int -> Parser ()
consumeSpace mSpaces = 
    whenJust mSpaces $ \spaces ->
        replicateM_ spaces (oneOf " \t")
--------------------------------------------------------------------------------
-- Markdown parser
--------------------------------------------------------------------------------

-- | Parser for 'Markdown'
markdownParser :: Parser Markdown
markdownParser = Markdown <$> manyTill (blockParser Nothing) eof

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
