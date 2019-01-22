{-# LANGUAGE LambdaCase #-}

module Parser.Text where

import           RIO                           hiding (many, try, (<|>))

import           Text.ParserCombinators.Parsec

import           Parser.Inline
import           Types

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

styledTextParser :: Parser Context
styledTextParser = do
    _         <- char '['
    symbols   <- manyTill (oneOf "*/-!^~$%&") space -- Need to check if there's missing symobols
    paragraph <- manyTill anyChar (try $ char ']')
    let style = mkStyle symbols
    segments  <- runSegmentParser paragraph
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
    lookAheadMaybe :: Parser a -> Parser (Maybe a)
    lookAheadMaybe parser = lookAhead . optionMaybe $ try parser

    checkWith :: String -> Parser a -> String -> Parser [Segment]
    checkWith str parser content' = do
        mResult <- lookAheadMaybe parser
        if isJust mResult
            then runSegmentParser content'
            else do
                someSymbol <- string str
                rest       <- many (noneOf "[")
                someParser $ content' <> someSymbol <> rest

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