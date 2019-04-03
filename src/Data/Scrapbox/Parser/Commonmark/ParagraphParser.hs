{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Parser.Commonmark.ParagraphParser
    ( toInlineBlocks
    ) where


import           RIO hiding (many, try, (<|>))

import qualified RIO.Text as T
import           Text.ParserCombinators.Parsec (Parser, anyChar, eof, many,
                                                many1, manyTill, noneOf, parse,
                                                string, try, (<?>), (<|>))

import           Data.Scrapbox.Parser.Utils (lookAheadMaybe)
import           Data.Scrapbox.Types (InlineBlock (..), Segment (..),
                                      Style (..))

strongParser :: Parser InlineBlock
strongParser = do
    str <- try (string "**") <|> string "__"
    text <- manyTill anyChar (try $ string str)
    return $ ITEM Bold [TEXT (fromString text)]

emphParser :: Parser InlineBlock
emphParser = do
    str  <- try (string "*") <|> string "_"
    text <- many1 $ noneOf str
    _    <- string str
    return $ ITEM Italic [TEXT (fromString text)]

strikeThroughParser :: Parser InlineBlock
strikeThroughParser = do
    _    <- string "~~"
    text <- manyTill anyChar (try $ string "~~")
    return $ ITEM StrikeThrough [TEXT (fromString text)]

-- | Parser for non-styled text
noStyleParser :: Parser InlineBlock
noStyleParser = ITEM NoStyle <$> extractNonStyledText
  where

    extractNonStyledText :: Parser [Segment]
    extractNonStyledText = go mempty

    go :: String -> Parser [Segment]
    go content = do
        someChar <- lookAheadMaybe
            (   try (string "__")
            <|> try (string "**")
            <|> try (string "_")
            <|> try (string "*")
            <|> try (string "~~")
            -- This is needed to avoid inifinite loop
            <|> try (string "~")
            <|> try (many1 (noneOf syntaxSymbols))
            )
        case someChar of
            Nothing   -> return [TEXT (fromString content)]
            -- Check if ahead content can be parsed as strong
            Just "**" -> checkWith "*" strongParser content
            Just "__" -> checkWith "_" strongParser content
            -- Check if ahead content can be parsed as emph
            Just "*"  -> checkWith "*" emphParser content
            Just "_"  -> checkWith "_" emphParser content
            -- Check if ahead content can be parsed as strikethrough
            Just "~~" -> checkWith "~~" strikeThroughParser content
            -- For everything else, consume until syntax
            Just "~"  -> do
                char <- anyChar
                rest <- many $ noneOf syntaxSymbols
                go $ content <> [char] <> rest
            Just _ -> do
                rest <- many $ noneOf syntaxSymbols
                go $ content <> rest

    -- Run parser on ahead content to see if it can be parsed, if not, consume the text
    checkWith :: String -> Parser a -> String -> Parser [Segment]
    checkWith symbolStr parser content = do
        canBeParsed <- isJust <$> lookAheadMaybe parser
        if canBeParsed
            then return [TEXT (fromString content)]
            else continue symbolStr content

    continue :: String -> String -> Parser [Segment]
    continue symbol curr = do
        someSymbol <- string symbol
        rest       <- many (noneOf syntaxSymbols)
        go $ curr <> someSymbol <> rest

    syntaxSymbols :: String
    syntaxSymbols = "_*~"

toInlineBlocks :: Text -> [InlineBlock]
toInlineBlocks text =
    either
        (const [ITEM NoStyle [TEXT text]])
        id
        (parse parser "Paragraph parser" (T.unpack text))
  where
    parser :: Parser [InlineBlock]
    parser = manyTill (
            try strikeThroughParser
        <|> try strongParser
        <|> try emphParser
        <|> try noStyleParser
        <?> "Cannot parse given paragraph"
        ) (try eof)
