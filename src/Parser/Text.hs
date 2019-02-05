{-| Module for 'ScrapText' parser
-}

{-# LANGUAGE LambdaCase #-}

module Parser.Text
    ( runScrapTextParser
    , runScrapTextParserM
    , scrapTextParser
    , styledTextParser
    , boldParser
    ) where

import           RIO                           hiding (many, try, (<|>))

import           Text.ParserCombinators.Parsec (ParseError, Parser, anyChar,
                                                char, eof, many, many1,
                                                manyTill, noneOf, oneOf, parse,
                                                space, string, try, unexpected,
                                                (<|>))

import           Parser.Inline                 (codeNotationParser,
                                                runInlineParserM)
import           Parser.Utils                  (lookAheadMaybe)
import           Types                         (Context (..), ScrapText (..),
                                                Segment (..), Style (..),
                                                StyleData (..), emptyStyle)
import           Utils                         (eitherM)

-- | Run 'ScrapText' parser on given 'String'
--
-- @
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
scrapTextParser = ScrapText <$> manyTill contextParser eof

-- | Context parser
contextParser :: Parser Context
contextParser =
        try boldParser
    <|> try styledTextParser
    <|> try noStyleParser

-- | Parser for bold text @[[Like this]]@
boldParser :: Parser Context
boldParser = do
    _         <- string "[["
    paragraph <- extractStr
    segments  <- runInlineParserM paragraph
    return $ Context Bold segments
  where
    extractStr :: Parser String
    extractStr = go mempty
    go :: String -> Parser String
    go content = do
        -- Check if we have enough closing brackets ahead
        hasEnoughClosingBracket <- isJust <$> lookAheadMaybe
            (manyTill anyChar (try $ char ']') *> manyTill anyChar (try $ string "]]"))
        if hasEnoughClosingBracket

            -- We do have enough closing brackets ahead, move on
            then do
                parsed <- many (noneOf  "]")
                symbol <- anyChar
                go $ content <> parsed <> [symbol]

            -- If not, consume until double closing bracket
            else do
                tillClose'' <- manyTill anyChar (try $ string "]]")
                return $ content <> tillClose''

-- | Parse styled text
--
-- @
-- Bold: [* Text]
-- Italic: [/ Text]
-- StrikeThrough: [- Text]
-- @
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
noStyleParser :: Parser Context
noStyleParser = Context NoStyle <$> extractNonStyledText
  where

    extractNonStyledText :: Parser [Segment]
    extractNonStyledText = go mempty

    go :: String -> Parser [Segment]
    go content = do
        someChar <- lookAheadMaybe
            (   try (string "[[")
            <|> try (string "[")
            <|> try (string "`")
            <|> try (many1 (noneOf "["))
            )
        case someChar of
            Nothing   -> runInlineParserM content

            -- Check if ahead content can be parsed as bold text
            Just "[[" -> checkWith "[[" boldParser content

            -- Check if ahead content can be parsed as custom styled text
            Just "["  -> checkWith "[" styledTextParser content

            -- Check if ahead content can be parsed as code notation
            Just "`" -> checkCodeNotation codeNotationParser content

            -- For everything else, consume until open bracket
            Just _ -> do
                rest <- many1 $ noneOf "["
                go $ content <> rest

    -- Run parser on ahead content to see if it can be parsed, if not, consume the text
    checkWith :: String -> Parser a -> String -> Parser [Segment]
    checkWith symbolStr parser content' = do
        canBeParsed <- isJust <$> lookAheadMaybe parser
        if canBeParsed
            then runInlineParserM content'
            else continue symbolStr "[" content'

    checkCodeNotation :: Parser a -> String -> Parser [Segment]
    checkCodeNotation parser content' = do
        canBeParsed <- isJust <$> lookAheadMaybe parser
        if canBeParsed
            then continue "`" "`" content'
            else continue "`" "[" content'

    continue :: String -> String -> String -> Parser [Segment]
    continue symbol till curr = do
        someSymbol <- string symbol
        rest       <- many (noneOf till)
        go $ curr <> someSymbol <> rest


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
