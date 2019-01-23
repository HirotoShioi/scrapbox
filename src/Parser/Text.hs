{-# LANGUAGE LambdaCase #-}

module Parser.Text where

import           RIO                           hiding (many, try, (<|>))

import           Text.ParserCombinators.Parsec

import           Parser.Inline
import           Types

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

scrapTextParser :: Parser ScrapText
scrapTextParser = ScrapText <$> manyTill contextParser eof

contextParser :: Parser Context
contextParser =
        try boldParser
    <|> try styledTextParser
    <|> try noStyleParser

boldParser :: Parser Context
boldParser = do
    _         <- string "[["
    paragraph <- manyTill anyChar (try $ string "]]")
    segments <- runSegmentParser paragraph
    return $ Context Bold segments

-- [ [a]]
-- shouldClose
-- [abc [def] fead]]
styledTextParser :: Parser Context
styledTextParser = do
    _         <- char '['
    symbols   <- manyTill (oneOf "*/-!^~$%&") space -- Need to check if there's missing symobols
    paragraph <- paraParser mempty
    let style = mkStyle symbols
    segments  <- runSegmentParser paragraph
    _         <- char ']'
    return $ Context style segments
  where

    mkStyle :: String -> Style
    mkStyle = \case
        "*"  -> Bold
        "-"  -> StrikeThrough
        "/"  -> Italic
        rest -> if any (`notElem` "*-/") rest
            then UserStyle (fromString rest)
            else mkCustomStyle rest emptyStyle

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

-- As you can see, this is very dangerous
-- Logic
-- First, check if there's any closing bracket '['
-- If yes, check if the extracted text has any open bracket
-- If no, consume until closing bracket
-- If yes, check if there's another closing bracket ahead
-- If yes, consume until ']' as well as ']' and continue parsing
-- If no, consume until ']' and return
paraParser :: String -> Parser String
paraParser content = do
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
                  -- Check if we have closing bracket for our parent
                  hasNextClosingBracket <- isJust <$> lookAheadMaybe
                       (manyTill anyChar (char ']') *> manyTill anyChar (char ']'))
                  if hasNextClosingBracket
                    -- We good, move on
                    then do
                        content2 <- many $ noneOf "]"
                        symbol   <- anyChar
                        paraParser $ content <> content2 <> [symbol]
                    else do
                        content3 <- many $ noneOf "]"
                        return $ content <> content3

noStyleParser :: Parser Context
noStyleParser = Context NoStyle <$> someParser mempty

someParser :: String -> Parser [Segment]
someParser content = do
    someChar <- lookAhead $ optionMaybe
        (   try (string "[[")
        <|> try (string "[")
        <|> try (many1 (noneOf "["))
        )
    case someChar of
        Nothing   -> runSegmentParser content
        Just "[[" -> checkWith "[[" boldParser content
        Just "["  -> checkWith "[" styledTextParser content
        Just _ -> do
            rest <- many1 $ noneOf "["
            someParser $ content <> rest

  where

    checkWith :: String -> Parser a -> String -> Parser [Segment]
    checkWith str parser content' = do
        mResult <- lookAheadMaybe parser
        if isJust mResult
            then runSegmentParser content'
            else do
                someSymbol <- string str
                rest       <- many (noneOf "[")
                someParser $ content' <> someSymbol <> rest

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

lookAheadMaybe :: Parser a -> Parser (Maybe a)
lookAheadMaybe parser = lookAhead . optionMaybe $ try parser

runSegmentParser :: String -> Parser [Segment]
runSegmentParser content' =
    eitherM
        (\_ -> unexpected "Failed to parse segments")
        return
        (return $ runInlineParser content')

eitherM :: Monad m => (a -> m c) -> (b -> m c) -> m (Either a b) -> m c
eitherM l r x = either l r =<< x

-- This is causing infinite loop
-- >>> parse boldParser' "Bold parser" "[[This is bold text]]"
-- I'm guessing there's a bug in 'textParser'
boldParser' :: Parser Context
boldParser' = do
    _         <- string "[["
    paragraph <- manyTill segmentParser (try $ string "]]")
    return $ Context Bold paragraph
