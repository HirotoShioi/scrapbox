{-| Module for 'ScrapText' parser
-}
{-# LANGUAGE LambdaCase #-}

module Parser.Text
    ( runScrapTextParser
    , runScrapTextParserM
    , scrapTextParser
    ) where

import           RIO                           hiding (many, try, (<|>))

import           Text.ParserCombinators.Parsec (ParseError, Parser, anyChar,
                                                char, eof, lookAhead, many,
                                                many1, manyTill, noneOf, oneOf,
                                                optionMaybe, parse, space,
                                                string, try, unexpected, (<|>))

import           Parser.Inline
import           Parser.Utils                  (lookAheadMaybe)
import           Types
import           Utils                         (eitherM)

-- | ScrapText parser
-- > runScrapTextParser "[* bold text] [- strikethrough text] [/ italic text] simple text [* test [link] test [buggy]"
-- Right
--     ( ScrapText
--         [ Context Bold [ SimpleText "bold text" ]
--         , Context NoStyle [ SimpleText " " ]
--         , Context StrikeThrough [ SimpleText "strikethrough text" ]
--         , Context NoStyle [ SimpleText " " ]
--         , Context Italic [ SimpleText "italic text" ]
--         , Context NoStyle [ SimpleText " simple text " ]
--         , Context Bold
--             [ SimpleText "test "
--             , Link Nothing ( Url "link" )
--             , SimpleText " test [buggy"
--             ]
--         ]
--     )
runScrapTextParser :: String -> Either ParseError ScrapText
runScrapTextParser = parse scrapTextParser "Scrap text parser"

-- | Monadic version of 'runScrapTextParser'
runScrapTextParserM :: String -> Parser ScrapText
runScrapTextParserM content =
    eitherM
        (\_ -> unexpected "Failed to parse scrap text")
        return
        (return $ runScrapTextParser content)

-- | Parser for scraptext
scrapTextParser :: Parser ScrapText
scrapTextParser = ScrapText <$> manyTill contextParser eof

-- | Context parser
contextParser :: Parser Context
contextParser =
        try boldParser
    <|> try styledTextParser
    <|> try noStyleParser

-- | Parser for bold text `[[Like this]]`
boldParser :: Parser Context
boldParser = do
    _         <- string "[["
    paragraph <- manyTill anyChar (try $ string "]]")
    segments <- runInlineParserM paragraph
    return $ Context Bold segments

-- | Parse styled text
--
-- Bold: `[* Text]`
--
-- Italic: `[/ Text]`
--
-- StrikeThrough: `[- Text]
styledTextParser :: Parser Context
styledTextParser = do
    _         <- char '['
    symbols   <- manyTill (oneOf "*/-!^~$%&") space -- Need to check if there's missing symobols
    paragraph <- extractParagraph
    let style = mkStyle symbols
    segments  <- runInlineParserM paragraph
    _         <- char ']'
    return $ Context style segments
  where
    -- Create style
    mkStyle :: String -> Style
    mkStyle = \case
        "*"  -> Bold
        "-"  -> StrikeThrough
        "/"  -> Italic
        rest -> if any (`notElem` "*-/") rest
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
-- First, check if there's any closing bracket '['
--
-- If yes, check if the extracted text has any open bracket
--
-- If no, consume until closing bracket
--
-- If yes, check if there's another closing bracket ahead
--
-- If yes, consume until ']' as well as ']' and continue parsing
--
-- If no, consume until ']' and return
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
                        -- We are good, move on
                        then do
                            tillClose' <- manyTill anyChar (try $ char ']')
                            go $ content <> tillClose'

                        -- If not (there's no closing bracket ahead), consume until closing bracket
                        else do
                            tillClose'' <- many $ noneOf "]"
                            return $ content <> tillClose''

-- | Parser for non-styled text
noStyleParser :: Parser Context
noStyleParser = Context NoStyle <$> extractNonStyledText
  where

    extractNonStyledText :: Parser [Segment]
    extractNonStyledText = go mempty

    go :: String -> Parser [Segment]
    go content = do
        someChar <- lookAhead $ optionMaybe
            (   try (string "[[")
            <|> try (string "[")
            <|> try (many1 (noneOf "["))
            )
        case someChar of
            Nothing   -> runInlineParserM content

            -- Check if ahead content can be parsed as bold text
            Just "[[" -> checkWith "[[" boldParser content

            -- Check if ahead content can be parsed as custom styled text
            Just "["  -> checkWith "[" styledTextParser content

            -- For everything else, consume until opening bracket
            Just _ -> do
                rest <- many1 $ noneOf "["
                go $ content <> rest

    -- Run parser on ahead content to see if it can be parsed, if not, consume the text
    checkWith :: String -> Parser a -> String -> Parser [Segment]
    checkWith symbolStr parser content' = do
        mResult <- lookAheadMaybe parser
        if isJust mResult
            then runInlineParserM content'
            else do
                someSymbol <- string symbolStr
                rest       <- many (noneOf "[")
                go $ content' <> someSymbol <> rest

--------------------------------------------------------------------------------
-- Needs attention
--------------------------------------------------------------------------------

-- This is causing infinite loop
-- >>> parse boldParser' "Bold parser" "[[This is bold text]]"
-- I'm guessing there's a bug in 'textParser'
-- boldParser' :: Parser Context
-- boldParser' = do
--     _         <- string "[["
--     paragraph <- manyTill segmentParser (try $ string "]]")
--     return $ Context Bold paragraph
