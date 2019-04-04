{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Parser.Commonmark.ParagraphParser
    ( toInlineBlocks
    , runParagraphParser
    ) where


import           RIO hiding (many, try, (<|>))

import qualified RIO.Text as T
import           Text.ParserCombinators.Parsec (ParseError, Parser, anyChar,
                                                eof, many, many1, manyTill,
                                                noneOf, parse, string, try,
                                                (<?>), (<|>))

import           Data.Scrapbox.Parser.Utils (lookAheadMaybe)
import           Data.Scrapbox.Types (InlineBlock (..), Segment (..),
                                      Style (..), StyleData(..), emptyStyle)

strongParser :: StyleData -> String -> Parser InlineBlock
strongParser styleData symbol = do
    str <- try (string "**") <|> string "__"
    let newSymbol = str <> symbol
    let withBold = styleData { sBold = True } 
    try (emphParser withBold newSymbol) 
        <|> try (strikeThroughParser withBold newSymbol) 
        <|> specialParser newSymbol withBold

emphParser :: StyleData -> String -> Parser InlineBlock
emphParser styleData symbol = do
    str  <- try (string "*") <|> string "_"
    let newSymbol = str <> symbol
    let withEmph = styleData { sItalic = True }
    try (strongParser withEmph newSymbol) 
        <|> try (strikeThroughParser withEmph newSymbol) 
        <|> specialParser newSymbol withEmph

specialParser :: String -> StyleData -> Parser InlineBlock
specialParser str styleData' = do
    text <- manyTill anyChar (try $ string str)
    when (null text) $ fail "Nothing to consume"
    return $ ITEM (CustomStyle styleData') [TEXT (fromString text)]

strikeThroughParser :: StyleData -> String -> Parser InlineBlock
strikeThroughParser styleData symbol = do
    str <- string "~~"
    let newSymbol = str <> symbol
    let withStrike = styleData { sStrikeThrough = True}
    try (strongParser withStrike newSymbol) 
        <|> try (emphParser withStrike newSymbol) 
        <|> specialParser newSymbol withStrike

-- | Parser for non-styled text
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
            Just "**" -> checkWith "*" (strongParser emptyStyle mempty) content
            Just "__" -> checkWith "_" (strongParser emptyStyle mempty) content
            -- Check if ahead content can be parsed as emph
            Just "*"  -> checkWith "*" (emphParser emptyStyle mempty) content
            Just "_"  -> checkWith "_" (emphParser emptyStyle mempty) content
            -- Check if ahead content can be parsed as strikethrough
            Just "~~" -> checkWith "~~" (strikeThroughParser emptyStyle mempty) content
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
runParagraphParser = parse parser "Paragraph parser"
  where
    parser :: Parser [InlineBlock]
    parser = manyTill (
            try (strikeThroughParser emptyStyle mempty)
        <|> try (strongParser emptyStyle mempty)
        <|> try (emphParser emptyStyle mempty)
        <|> try noStyleParser
        <?> "Cannot parse given paragraph"
        ) (try eof)

toInlineBlocks :: Text -> [InlineBlock]
toInlineBlocks text =
    either
        (const [ITEM NoStyle [TEXT text]])
        id
        (runParagraphParser (T.unpack text))
