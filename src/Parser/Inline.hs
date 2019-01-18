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
                                                (<?>), (<|>))

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
    (mName, someLink) <- if length contents <= 1
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
simpleTextParser = do
    content <- many1 $ noneOf "[`#"
    someChar <- lookAheadMaybe anyChar
    case someChar of

        -- Nothing is behind
        Nothing  -> return $ simpleText content

        -- Could be link so try to parse it
        Just '[' -> do
            mLink <- lookAheadMaybe linkParser
            if isJust mLink
                then return $ simpleText content
                else do
                    rest <- many1 $ noneOf "`#"
                    return $ simpleText $ content <> rest

        -- Could be code notation so try to parse it
        Just '`' -> do
            mCodeNotation <- lookAheadMaybe codeNotationParser
            if isJust mCodeNotation
                then return $ simpleText content
                else do
                    rest <- many1 $ noneOf "[#"
                    return $ simpleText $ content <> rest

        -- For everything else, return the content
        Just _ -> return $ simpleText content
  where
    lookAheadMaybe :: Parser a -> Parser (Maybe a)
    lookAheadMaybe parser = lookAhead . optionMaybe $ try parser

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
inlineParser = many segmentParser -- May want to switch over to many1 to make it fail

-- | Function to test whether given 'String' can be properly parsed
testInlineParser :: String -> Either ParseError [Segment]
testInlineParser = parse inlineParser "Inline text parser"

-- > textParser "hello [hello yahoo link www.yahoo.co.jp] [hello] []"
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
