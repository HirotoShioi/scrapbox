{-# LANGUAGE LambdaCase #-}

-- | Module for 'ScrapText' parser
module Data.Scrapbox.Parser.Scrapbox.ScrapText
  ( runScrapTextParser,
    runScrapTextParserM,
    scrapTextParser,
    styledTextParser,
    boldParser,
    noStyleParser,
    extractParagraph,
  )
where

import Data.Scrapbox.Parser.Scrapbox.Span (runSpanParserM)
import Data.Scrapbox.Parser.Utils (lookAheadMaybe)
import Data.Scrapbox.Types
  ( InlineBlock (..),
    Level (..),
    ScrapText (..),
    Segment (..),
    Style (..),
  )
import RIO hiding (many, try)
import RIO.List (sort)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    anyChar,
    between,
    char,
    eof,
    many,
    many1,
    manyTill,
    noneOf,
    oneOf,
    parse,
    satisfy,
    string,
    try,
    unexpected,
  )

-- | Run 'ScrapText' parser on given 'String'
--
-- >>> runScrapTextParser "[* bold text] [- strikethrough text] [/ italic text] simple text [* test [link] test [buggy]"
-- Right
--     ( ScrapText
--         [ SPAN [ Bold ] [ TEXT "bold text" ]
--         , SPAN [] [ TEXT " " ]
--         , SPAN [ StrikeThrough ] [ TEXT "strikethrough text" ]
--         , SPAN [] [ TEXT " " ]
--         , SPAN [ Italic ] [ TEXT "italic text" ]
--         , SPAN [] [ TEXT " simple text " ]
--         , SPAN [ Bold ]
--             [ TEXT "test "
--             , LINK Nothing ( Url "link" )
--             , TEXT " test [buggy"
--             ]
--         ]
--     )
runScrapTextParser ::
  -- @
  String -> Either ParseError ScrapText
runScrapTextParser = parse scrapTextParser "Scrap text parser"

-- | Monadic version of 'runScrapTextParser'
runScrapTextParserM :: String -> Parser ScrapText
runScrapTextParserM content =
  either
    (\_ -> unexpected "Failed to parse scrap text")
    return
    (runScrapTextParser content)

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
  _ <- string "[["
  paragraph <- extractStr
  segments <- runSpanParserM paragraph
  _ <- string "]]"
  return $ SPAN [Bold] segments
  where
    extractStr :: Parser String
    extractStr = go mempty
    go :: String -> Parser String
    go content = do
      -- Check if we have enough closing brackets ahead
      aheadContent <-
        lookAheadMaybe $
          try (string "]]]")
            <|> try (string "]]")
            <|> try (many1 $ noneOf "]")
      case aheadContent of
        Nothing -> return content
        Just "]]]" -> do
          symbol <- anyChar
          return $ content <> [symbol]
        Just "]]" -> return content
        Just _ -> do
          more <- many1 $ noneOf "]"
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
  _ <- char '['
  -- Need to check if there's missing symobols
  symbols <- manyTill (oneOf "*/-!^~$%&?#") (satisfy (== ' '))
  paragraph <- extractParagraph
  let style = mkStyle symbols
  segments <- runSpanParserM paragraph
  _ <- char ']'
  return $ SPAN style segments
  where
    -- Create style
    mkStyle :: String -> [Style]
    mkStyle = \case
      "*" -> [Bold]
      "-" -> [StrikeThrough]
      "/" -> [Italic]
      rest ->
        if any (`notElem` "*-/") rest -- Need Header style
          then [UserStyle (fromString rest)]
          else mkCustomStyle rest mempty
    -- Create custom style
    mkCustomStyle :: String -> [Style] -> [Style]
    mkCustomStyle str styles
      | any (`elem` "-") str =
        mkCustomStyle (filter (/= '-') str) (StrikeThrough : styles)
      | any (`elem` "/") str =
        mkCustomStyle (filter (/= '/') str) (Italic : styles)
      | any (`elem` "*") str =
        let level = length $ filter (== '*') str
         in mkCustomStyle (filter (/= '*') str) (Sized (Level level) : styles)
      | null str = sort styles
      | otherwise = sort styles

-- | Extract paragraph that are surronded by brackets
extractParagraph :: Parser String
extractParagraph = go mempty
  where
    -- As you can see, this is very dangerous
    -- Logic test
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
    go :: String -> Parser String
    go content = do
      mContent1 <- lookAheadMaybe (manyTill anyChar (char ']'))
      case mContent1 of
        Nothing -> return content
        Just content1 -> do
          -- Check if there's no open bracket
          let hasNoOpenBracket = '[' `notElem` content1
          if hasNoOpenBracket
            then do
              tillClose <- many $ noneOf "]"
              return $ content <> tillClose
            else do
              -- Check if we have closing bracket for our parent bracket
              hasNextClosingBracket <-
                isJust
                  <$> lookAheadMaybe
                    (manyTill anyChar (char ']') *> manyTill anyChar (char ']'))
              if hasNextClosingBracket
                then-- We do have 2 closing brackets ahead, move on
                do
                  tillClose' <- many (noneOf "]")
                  symbol <- anyChar
                  go $ content <> tillClose' <> [symbol]
                else-- If not (there's no closing bracket ahead for parent bracket)
                -- consume until closing bracket
                do
                  tillClose'' <- many $ noneOf "]"
                  return $ content <> tillClose''

-- | Parser for non-styled text
noStyleParser :: Parser InlineBlock
noStyleParser = SPAN [] <$> extractNonStyledText
  where
    extractNonStyledText :: Parser [Segment]
    extractNonStyledText = go mempty
    go :: String -> Parser [Segment]
    go content = do
      someChar <-
        lookAheadMaybe
          ( try (string "[[")
              <|> try (string "[")
              <|> try (string "`")
              <|> try (many1 (noneOf syntaxSymbols))
          )
      case someChar of
        Nothing -> runSpanParserM content
        -- Check if ahead content can be parsed as bold text
        Just "[[" -> checkWith "[[" boldParser content
        -- Check if ahead content can be parsed as custom styled text
        Just "[" -> checkWith "[" styledTextParser content
        -- Check if ahead content can be parsed as code notation
        Just "`" -> checkWith "`" codeNotationParser content
        -- For everything else, consume until open bracket
        Just _ -> do
          rest <- many1 $ noneOf syntaxSymbols
          go $ content <> rest
    -- Run parser on ahead content to see if it can be parsed, if not, consume the text
    checkWith :: String -> Parser a -> String -> Parser [Segment]
    checkWith symbolStr parser content = do
      canBeParsed <- isJust <$> lookAheadMaybe parser
      if canBeParsed
        then runSpanParserM content
        else continue symbolStr content
    continue :: String -> String -> Parser [Segment]
    continue symbol curr = do
      someSymbol <- string symbol
      rest <- many (noneOf syntaxSymbols)
      go $ curr <> someSymbol <> rest
    syntaxSymbols :: String
    syntaxSymbols = "[`"

-- | Parser for 'CODE_NOTATION'
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
