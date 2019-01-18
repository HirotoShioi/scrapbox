module Parser.Inline 
    ( inlineParser
    , testInlineParser
    ) where

import           RIO                           hiding (many, try, (<|>))
import           RIO.List                      (headMaybe, initMaybe, lastMaybe)

import           Data.String                   (fromString)
import qualified Data.Text                     as T
import           Text.ParserCombinators.Parsec (ParseError, Parser, anyChar,
                                                between, char, lookAhead, many,
                                                many1, noneOf, optionMaybe,
                                                parse, sepBy1, space, try,
                                                (<?>), (<|>), manyTill, eof, oneOf)

import           Types                         (Segment (..), Url (..))

--------------------------------------------------------------------------------
-- Smart contstructors (do not export these)
--------------------------------------------------------------------------------

-- | Used to create 'SimpleText'
simpleText :: String -> Segment
simpleText = SimpleText . fromString

-- | Used to create 'CodeNotation'
codeNotation :: String -> Segment
codeNotation = CodeNotation . fromString

-- | Used to create 'HashTag'
hashtag :: String -> Segment
hashtag = HashTag . fromString

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

-- | Parser for 'CodeNotation'
codeNotationParser :: Parser Segment
codeNotationParser = do
    content <- between (char '`') (char '`') $ many1 (noneOf "`")
    return $ codeNotation content

-- | Parser for 'HashTag'
hashTagParser :: Parser Segment
hashTagParser = do
    _ <- char '#'
    content <- many1 (noneOf " ")
    return $ hashtag content

-- | Parser for 'Link'
linkParser :: Parser Segment
linkParser = do
    contents <- between (char '[') (char ']') $ sepBy1 (many1 $ noneOf "] ") space
    (mName, someLink) <- if length contents == 1
        then do
            linkContent <- getElement $ headMaybe contents
            return (Nothing, linkContent)
        else do
            nameContent <- getElement $ initMaybe contents
            linkContent <- getElement $ lastMaybe contents
            let name = T.strip $ fromString $
                    foldr (\someText acc -> someText <> " " <> acc) mempty nameContent
            return (Just name, linkContent)
    return $ Link mName (Url $ fromString someLink)
  where
    getElement :: (Monad m) => Maybe a -> m a
    getElement mf = fromMaybeM (fail "failed to parse link content") (return mf)

-- | Parser fro 'SimpleText'
simpleTextParser :: Parser Segment
simpleTextParser = simpleText <$> textParser mempty

textParser :: String -> Parser String
textParser content = do
    someChar <- lookAheadMaybe anyChar
    case someChar of

        -- Nothing is ahead of it, the work is done
        Nothing  -> return content

        -- Could be link so try to parse it
        Just '[' -> checkWith linkParser content

        -- Could be code notation so try to parse it
        Just '`' -> checkWith codeNotationParser content

        -- Could be hashtag, so try to parse it
        Just '#' -> checkWith hashTagParser content

        -- For everything else, parse until it hits the syntax symbol
        Just _   -> do
            text <- many $ noneOf syntaxSymbol
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

    lookAheadMaybe :: Parser a -> Parser (Maybe a)
    lookAheadMaybe parser = lookAhead . optionMaybe $ try parser

    syntaxSymbol :: String
    syntaxSymbol = "[`#"

-- | Parse inline text into list of 'Segment'
segmentParser :: Parser Segment
segmentParser =
        try codeNotationParser
    <|> try linkParser
    <|> try hashTagParser
    <|> try simpleTextParser
    <?> "failed to parse segment"

-- | Parser for inline text
inlineParser :: Parser [Segment]
inlineParser = manyTill segmentParser eof-- May want to switch over to many1 to make it fail

-- | Function to test whether given 'String' can be properly parsed
testInlineParser :: String -> Either ParseError [Segment]
testInlineParser = parse inlineParser "Inline text parser"

-- > textInlineParser "hello [hello yahoo link www.yahoo.co.jp] [hello] [] `failed code [failed url #someHashtag"
-- Right
--     [ SimpleText "hello "
--     , Link ( Just "hello yahoo link" ) ( Url "www.yahoo.co.jp" )
--     , SimpleText " "
--     , Link Nothing ( Url "hello" )
--     , SimpleText " []"
--     ]

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- | Monadic maybe
maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j x = maybe n j =<< x

-- | Monadic fromMaybe
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM n = maybeM n return
