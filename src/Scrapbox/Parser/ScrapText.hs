{-| Module for 'ScrapText' parser
-}

{-# LANGUAGE LambdaCase #-}

module Scrapbox.Parser.ScrapText
    ( runScrapTextParser
    , runScrapTextParserM
    , scrapTextParser
    , styledTextParser
    , boldParser
    , noStyleParser
    , extractParagraph
    ) where

import           RIO                           hiding (many, try, (<|>))

import           Text.ParserCombinators.Parsec (ParseError, Parser, anyChar,
                                                between, char, eof, many, many1,
                                                manyTill, noneOf, oneOf, parse,
                                                space, string, try, unexpected,
                                                (<|>))

import           Scrapbox.Parser.Item          (runItemParserM)
import           Scrapbox.Parser.Utils         (lookAheadMaybe)
import           Scrapbox.Types                (InlineBlock (..),
                                                ScrapText (..), Segment (..),
                                                Style (..), StyleData (..),
                                                emptyStyle)
import           Scrapbox.Utils                (eitherM)

-- | Run 'ScrapText' parser on given 'String'
--
-- @
-- > runScrapTextParser "[* bold text] [- strikethrough text] [/ italic text] simple text [* test [link] test [buggy]"
-- Right
--     ( ScrapText
--         [ InlineBlock Bold [ SimpleText "bold text" ]
--         , InlineBlock NoStyle [ SimpleText " " ]
--         , InlineBlock StrikeThrough [ SimpleText "strikethrough text" ]
--         , InlineBlock NoStyle [ SimpleText " " ]
--         , InlineBlock Italic [ SimpleText "italic text" ]
--         , InlineBlock NoStyle [ SimpleText " simple text " ]
--         , InlineBlock Bold
--             [ SimpleText "test "
--             , Link Nothing ( Url "link" )
--             , SimpleText " test [buggy"
--             ]
--         ]
--     )
-- @
runScrapTextParser :: String -> Either ParseError ScrapText
runScrapTextParser = parse scrapTextParser "Scrap text parser"

-- | Monadic version of 'runScrapTextParser'
runScrapTextParserM :: String -> Parser ScrapText
runScrapTextParserM content =
    eitherM
        (\_ -> unexpected "Failed to parse scrap text")
        return
        (return $ runScrapTextParser content)

-- | Parser for 'ScrapText'
scrapTextParser :: Parser ScrapText
scrapTextParser = ScrapText <$> manyTill inlineBlockParser eof

-- | InlineBlock parser
inlineBlockParser :: Parser InlineBlock
inlineBlockParser =
        try codeNotationParser
    <|> try mathExpressionParser
    <|> try boldParser
    <|> try styledTextParser
    <|> try noStyleParser

-- | Parser for bold text @[[Like this]]@
boldParser :: Parser InlineBlock
boldParser = do
    _         <- string "[["
    paragraph <- extractStr
    segments  <- runItemParserM paragraph
    _         <- string "]]"
    return $ ITEM Bold segments
  where
    extractStr :: Parser String
    extractStr = go mempty
    go :: String -> Parser String
    go content = do
        -- Check if we have enough closing brackets ahead
        aheadContent <- lookAheadMaybe $
                try (string "]]]")
            <|> try (string "]]")
            <|> try (many1 $ noneOf "]")
        case aheadContent of
            Nothing -> return content
            Just "]]]" -> do
                symbol <- anyChar
                return $ content <> [symbol]
            Just "]]"  -> return content
            Just _     -> do
                more   <- many1 $ noneOf "]"
                doubleSymbol <- isJust <$> lookAheadMaybe (try (string "]]"))
                if doubleSymbol
                    then go $ content <> more
                    else do
                        symbol <- anyChar
                        go $ content <> more <> [symbol]

-- | Parse styled text
--
-- @
-- Bold: [* Text]
-- Italic: [/ Text]
-- StrikeThrough: [- Text]
-- @
styledTextParser :: Parser InlineBlock
styledTextParser = do
    _         <- char '['
     -- Need to check if there's missing symobols
    symbols   <- manyTill (oneOf "*/-!^~$%&?") space
    paragraph <- extractParagraph
    let style = mkStyle symbols
    segments  <- runItemParserM paragraph
    _         <- char ']'
    return $ ITEM style segments
  where
    -- Create style
    mkStyle :: String -> Style
    mkStyle = \case
        "*"  -> Bold
        "-"  -> StrikeThrough
        "/"  -> Italic
        rest -> if any (`notElem` "*-/") rest -- Need Header style
            then UserStyle (fromString rest)
            else mkCustomStyle rest emptyStyle

    -- Create custom style
    mkCustomStyle :: String -> StyleData -> Style
    mkCustomStyle str styleData
        | any (`elem` "-") str =
            mkCustomStyle (filter (/= '-') str) styleData { sStrikeThrough = True}
        | any (`elem` "/") str =
            mkCustomStyle (filter (/= '/') str) styleData { sItalic = True}
        | any (`elem` "*") str =
            let headerSize = length $ filter (== '*') str
            in mkCustomStyle (filter (/= '*') str) $ styleData { sHeaderSize = headerSize}
        | null str             = CustomStyle styleData
        | otherwise            = CustomStyle styleData

-- | Extract paragraph that are surronded by brackets
-- As you can see, this is very dangerous
-- Logic
--
-- @
-- 1. First, check if there's any closing bracket ']'
-- 2. If 1 is True, check if the extracted text has any open bracket '['
-- 3. If 1 is False, return the current string
--
-- 4. If 2 is True, consume until closing bracket
-- 5. If 2 is False, check if there's another closing bracket ahead
--
-- 6. If 5 is True, consume until ']' as well as ']' and continue parsing
-- 7. If 5 is False, consume until ']' and return
-- @
extractParagraph :: Parser String
extractParagraph = go mempty
  where
    go :: String -> Parser String
    go content = do
        mContent1 <- lookAheadMaybe (manyTill anyChar (char ']'))
        case mContent1 of
            Nothing       -> return content
            Just content1 -> do
                -- Check if there's no open bracket
                let hasNoOpenBracket = '['  `notElem` content1
                if hasNoOpenBracket
                    then do
                        tillClose <- many $ noneOf "]"
                        return $ content <> tillClose
                    else do
                    -- Check if we have closing bracket for our parent bracket
                    hasNextClosingBracket <- isJust <$> lookAheadMaybe
                        (manyTill anyChar (char ']') *> manyTill anyChar (char ']'))
                    if hasNextClosingBracket
                        -- We do have 2 closing brackets ahead, move on
                        then do
                            tillClose' <- many (noneOf  "]")
                            symbol     <- anyChar
                            go $ content <> tillClose' <> [symbol]
                        -- If not (there's no closing bracket ahead for parent bracket)
                        -- consume until closing bracket
                        else do
                            tillClose'' <- many $ noneOf "]"
                            return $ content <> tillClose''

-- | Parser for non-styled text
noStyleParser :: Parser InlineBlock
noStyleParser = ITEM NoStyle <$> extractNonStyledText
  where

    extractNonStyledText :: Parser [Segment]
    extractNonStyledText = go mempty

    go :: String -> Parser [Segment]
    go content = do
        someChar <- lookAheadMaybe
            (   try (string "[[")
            <|> try (string "[")
            <|> try (string "`")
            <|> try (many1 (noneOf "[`"))
            )
        case someChar of
            Nothing   -> runItemParserM content
            -- Check if ahead content can be parsed as bold text
            Just "[[" -> checkWith "[[" boldParser content
            -- Check if ahead content can be parsed as custom styled text
            Just "["  -> checkWith "[" styledTextParser content
            -- Check if ahead content can be parsed as code notation
            Just "`"  -> checkWith "`" codeNotationParser content
            -- For everything else, consume until open bracket
            Just _ -> do
                rest <- many1 $ noneOf "[`"
                go $ content <> rest

    -- Run parser on ahead content to see if it can be parsed, if not, consume the text
    checkWith :: String -> Parser a -> String -> Parser [Segment]
    checkWith symbolStr parser content' = do
        canBeParsed <- isJust <$> lookAheadMaybe parser
        if canBeParsed
            then runItemParserM content'
            else continue symbolStr content'

    continue :: String -> String -> Parser [Segment]
    continue symbol curr = do
        someSymbol <- string symbol
        rest       <- many (noneOf "[`")
        go $ curr <> someSymbol <> rest

-- | Parser for 'CODENOTATION'
codeNotationParser :: Parser InlineBlock
codeNotationParser = do
    content <- between (char '`') (char '`') $ many1 (noneOf "`")
    return $ CODE_NOTATION $ fromString content

-- | Parser for 'MATH_EXPRESSION'
mathExpressionParser :: Parser InlineBlock
mathExpressionParser = do
    content <- between (string "[$ ") (char ']') $ many1 (noneOf "]")
    return $ MATH_EXPRESSION $ fromString content

--------------------------------------------------------------------------------
-- Needs attention
--------------------------------------------------------------------------------

-- This is causing infinite loop
-- >>> parse boldParser' "Bold parser" "[[This is bold text]]"
-- I'm guessing there's a bug in 'textParser'
-- boldParser' :: Parser InlineBlock
-- boldParser' = do
--     _         <- string "[["
--     paragraph <- manyTill segmentParser (try $ string "]]")
--     return $ InlineBlock Bold paragraph
