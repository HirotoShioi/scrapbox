{-| This module exports a parser which parses the content of PARAGRAPH in CMark
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Parser.Commonmark.ParagraphParser
    ( toInlineBlocks
    , runParagraphParser
    ) where


import           RIO hiding (many, try)

import qualified RIO.Text as T
import           Text.ParserCombinators.Parsec (ParseError, Parser, anyChar,
                                                eof, many, many1, manyTill,
                                                noneOf, parse, string, try,
                                                unexpected, (<?>))

import           Data.Scrapbox.Parser.Utils (lookAheadMaybe)
import           Data.Scrapbox.Types (InlineBlock (..), Segment (..),
                                      Style (..))

type Symbol = String

-- | Strong parser
strongParser :: [Style] -> Symbol -> Parser InlineBlock
strongParser styles symbol = do
    str <- try (string "**") <|> string "__"
    let newSymbol = str <> symbol
    let withBold = Bold : styles -- Check!
    try (emphParser withBold newSymbol)
        <|> try (strikeThroughParser withBold newSymbol)
        <|> textParser withBold newSymbol

-- | Emph sparser
emphParser :: [Style] -> Symbol -> Parser InlineBlock
emphParser styles symbol = do
    str  <- try (string "*") <|> string "_"
    let newSymbol = str <> symbol
    let withEmph = Italic : styles
    try (strongParser withEmph newSymbol)
        <|> try (strikeThroughParser withEmph newSymbol)
        <|> textParser withEmph newSymbol

-- | Strikethrough parser
strikeThroughParser :: [Style] -> Symbol -> Parser InlineBlock
strikeThroughParser styles symbol = do
    str <- string "~~"
    let newSymbol = str <> symbol
    let withStrike = StrikeThrough : styles
    try (strongParser withStrike newSymbol)
        <|> try (emphParser withStrike newSymbol)
        <|> textParser withStrike newSymbol

-- | Parser which takes a symbol and 'StyleData'
textParser :: [Style] -> Symbol -> Parser InlineBlock
textParser styles symbol = do
    text <- manyTill anyChar (try $ string symbol)
    when (null text) $ unexpected "Text is empty, nothing to consume"
    return $ ITEM styles [TEXT (fromString text)]

-- | Parser for non-styled text
noStyleParser :: Parser InlineBlock
noStyleParser = ITEM [] <$> extractNonStyledText
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
            Just "**" -> checkWith "*" (strongParser mempty mempty) content
            Just "__" -> checkWith "_" (strongParser mempty mempty) content
            -- Check if ahead content can be parsed as emph
            Just "*"  -> checkWith "*" (emphParser mempty mempty) content
            Just "_"  -> checkWith "_" (emphParser mempty mempty) content
            -- Check if ahead content can be parsed as strikethrough
            Just "~~" -> checkWith "~~" (strikeThroughParser mempty mempty) content
            -- For everything else, consume until syntax
            Just "~"  -> do
                char <- anyChar
                rest <- many $ noneOf syntaxSymbols
                go $ content <> [char] <> rest
            Just _ -> do
                rest <- many $ noneOf syntaxSymbols
                go $ content <> rest

    -- Run parser on ahead content to see if it can be parsed, if not, consume
    -- the text
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

-- | Parse given 'String' into @[InlineBlock]@
runParagraphParser :: String -> Either ParseError [InlineBlock]
runParagraphParser =  parse parser "Paragraph parser"
  where
    parser :: Parser [InlineBlock]
    parser = manyTill
          ( try (strikeThroughParser mempty mempty)
        <|> try (strongParser mempty mempty)
        <|> try (emphParser mempty mempty)
        <|> try noStyleParser
        <?> "Cannot parse given paragraph"
          ) (try eof)

-- | Convert given 'Text' into @[InlineBlock]@
toInlineBlocks :: Text -> [InlineBlock]
toInlineBlocks text = either
    (const [ITEM [] [TEXT text]])
    id
    (runParagraphParser (T.unpack text))
