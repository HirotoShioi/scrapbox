{-| Span parser module
-}

module Data.Scrapbox.Parser.Scrapbox.Span
    ( runSpanParser
    , runSpanParserM
    , linkParser
    ) where

import           Data.Char (isSpace)
import           Data.Scrapbox.Parser.Utils (lookAheadMaybe)
import           Data.Scrapbox.Types (Segment (..), Url (..))
import           Data.Scrapbox.Utils (isURL)
import           RIO hiding (many, try)
import           RIO.List (headMaybe, lastMaybe)
import qualified RIO.Text as T
import           Text.ParserCombinators.Parsec (ParseError, Parser, anyChar,
                                                between, char, eof, many, many1,
                                                manyTill, noneOf, oneOf, parse,
                                                satisfy, try, unexpected, (<?>))
--------------------------------------------------------------------------------
-- Smart contstructors
-- Do not export these. These should only be used within this module.
--------------------------------------------------------------------------------

-- | Used to create 'TEXT'
simpleText :: String -> Segment
simpleText = TEXT . fromString

-- | Used to create 'HASHTAG'
hashtag :: String -> Segment
hashtag = HASHTAG . fromString

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

-- | Parser for 'HASHTAG'
hashTagParser :: Parser Segment
hashTagParser = do
    _ <- char '#'
    content <- many1 $ satisfy (\c -> (not . isSpace) c && c `notElem` "[")
    return $ hashtag content

-- | Parser for 'TEXT'
simpleTextParser :: Parser Segment
simpleTextParser = simpleText <$> textParser mempty

-- Something is wrong, its causing infinite loop
-- | Parser for 'TEXT'
textParser :: String -> Parser String
textParser content = do
    someChar <- lookAheadMaybe anyChar
    case someChar of
        -- Nothing is ahead of it, the work is done
        Nothing  -> return content
        -- Could be link so try to parse it
        Just '[' -> checkWith linkParser content
        -- Could be hashtag, so try to parse it
        Just '#' -> checkWith hashTagParser content
        -- For everything else, parse until it hits the syntax symbol
        Just _   -> do
            text <- many1 $ noneOf syntaxSymbol
            textParser text
  where
    -- Check if the ahead content can be parsed by the given parser,
    -- if not, consume them as simpletext until next symbol
    checkWith :: Parser a -> String -> Parser String
    checkWith parser content' = do
        mResult <- lookAheadMaybe parser
        if isJust mResult
        then return content
        else do
            someSymbol <- oneOf syntaxSymbol
            rest       <- many $ noneOf syntaxSymbol
            textParser $ content' <> [someSymbol] <> rest

    syntaxSymbol :: String
    syntaxSymbol = "[#"

-- | Parse inline text into list of 'Segment'
segmentParser :: Parser Segment
segmentParser =
        try linkParser
    <|> try hashTagParser
    <|> try simpleTextParser
    <?> "failed to parse segment"

-- | Parser for inline text
spanParser :: Parser [Segment]
spanParser = manyTill segmentParser eof

-- | Run inline text parser on given 'String'
--
-- @
-- > runInlineParser "hello [hello yahoo link http://www.yahoo.co.jp] [hello] [] `weird code [weird url #someHashtag"
-- Right
--     [ SimpleText "hello "
--     , Link ( Just "hello yahoo link" ) ( Url "http://www.yahoo.co.jp" )
--     , SimpleText " "
--     , Link Nothing ( Url "hello" )
--     , SimpleText " [] `weird code [weird url "
--     , HashTag "someHashtag"
--     ]
-- @
runSpanParser :: String -> Either ParseError [Segment]
runSpanParser = parse spanParser "Inline text parser"

-- | Monadic version of 'runInlineParser'
runSpanParserM :: String -> Parser [Segment]
runSpanParserM content =
    either
        (\_ -> unexpected "Failed to parse inline text")
        return
        (runSpanParser content)

--------------------------------------------------------------------------------
-- Link parser
--------------------------------------------------------------------------------
linkParser :: Parser Segment
linkParser =
       try linkParser1
   <|> try linkParser2
   <|> try linkParser3

-- Parses [www.somelink.com this is some link]
linkParser1 :: Parser Segment
linkParser1 = do
    _    <- char '['
    rest <- manyTill anyChar (try $ char ']')
    uri  <- maybe
        (unexpected "No elements")
        return
        (headMaybe $ words rest)
    name <- maybe
        (unexpected "Uri does not exist on the extracted text")
        return
        (T.stripStart <$> T.stripPrefix (fromString uri) (fromString rest))
    if isURL uri && hasNoTrailingSpaces rest && (not . T.null $ name)
        then return $ LINK (Just name) (Url $ fromString uri)
        else unexpected "Not a link"
  where
    hasNoTrailingSpaces :: String -> Bool
    hasNoTrailingSpaces str =
        let txt = T.pack str
        in T.stripEnd txt == txt

-- Parses [this is some link www.somelink.com]
linkParser2 :: Parser Segment
linkParser2 = do
    _    <- char '['
    rest <- manyTill anyChar (try $ char ']')
    uri  <- maybe
        (unexpected "No elements")
        return
        (lastMaybe $ words rest)
    name <- maybe
        (unexpected "Uri does not exist on the extracted text")
        return
        (T.stripEnd <$> T.stripSuffix (fromString uri) (fromString rest))
    if isURL uri && hasNoTrailingSpaces rest && (not . T.null $ name)
        then return $ LINK (Just name) (Url $ fromString uri)
        else unexpected "Not a link"
  where
    hasNoTrailingSpaces :: String -> Bool
    hasNoTrailingSpaces str =
        let txt = T.pack str
        in T.stripEnd txt == txt

-- Parses [this is some link]
linkParser3 :: Parser Segment
linkParser3 = do
    content <- between (char '[') (char ']') $  many1 $ noneOf "]"
    if hasLink content && any isSpace content
        then unexpected "Not a link"
        else return $ LINK Nothing (Url $ fromString content)
  where
    hasLink :: String -> Bool
    hasLink [] = False
    hasLink whole@(_:xs)
      | isURL (takeWhile (not . isSpace) whole) = True
      | otherwise                               = hasLink xs

