{-| Test suites for 'runSpanParser'
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.Span
    ( spanParserSpec
    ) where

import           RIO hiding (assert)

import           RIO.List (headMaybe)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Monadic (assert, monadicIO)

import           Data.Scrapbox (Segment (..), Url (..))
import           Data.Scrapbox.Internal (isHashTag, isLink, isText,
                                         runSpanParser)
import           TestScrapboxParser.Utils (ScrapboxSyntax (..), checkContent,
                                           checkParsed, propParseAsExpected)
import           Utils (NonEmptyPrintableString (..), genMaybe,
                        genPrintableText, genPrintableUrl, genText,
                        shouldParseSpec, whenRight)

-- | Spec for inline text parser
spanParserSpec :: Spec
spanParserSpec =
    describe "Span parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runSpanParser

        prop "should return non-empty list of segments if given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runSpanParser $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \parsedContent ->
                    assert $ not $ null parsedContent

        it "should parse given text as expected" $
            propParseAsExpected exampleText expected runSpanParser

        -- Span specs
        describe "Spans" $ modifyMaxSuccess (const 200) $ do
            textSpec
            linkSpec
            hashTagSpec
  where
    exampleText :: String
    exampleText = "hello [hello yahoo link http://www.yahoo.co.jp] [hello] [] `partial code [partial url #someHashtag"

    expected :: [Segment]
    expected =
        [ TEXT "hello "
        , LINK ( Just "hello yahoo link" ) ( Url "http://www.yahoo.co.jp" )
        , TEXT " "
        , LINK Nothing ( Url "hello" )
        , TEXT " [] `partial code [partial url "
        , HASHTAG "someHashtag"
        ]

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

newtype TextSpan = TextSpan Text
    deriving Show

instance Arbitrary TextSpan where
    arbitrary = TextSpan <$> genPrintableText

instance ScrapboxSyntax TextSpan where
    render (TextSpan txt)     = txt
    getContent (TextSpan txt) = txt


--- Text
textSpec :: Spec
textSpec = describe "TEXT" $ do
    prop "should parse text as TEXT" $
        \(someText :: TextSpan) ->
            checkParsed someText runSpanParser headMaybe isText
    prop "should preserve its content" $
        \(someText ::TextSpan) ->
            checkContent someText runSpanParser
                (\segments -> do
                  guard $ length segments == 1
                  segment <- headMaybe segments
                  getText segment
                )
  where
    getText :: Segment -> Maybe Text
    getText (TEXT text) = Just text
    getText _           = Nothing

--------------------------------------------------------------------------------
-- Link
--------------------------------------------------------------------------------

data LinkSpan = LinkSpan !(Maybe Text) !Text
    deriving Show

instance Arbitrary LinkSpan where
    arbitrary = LinkSpan <$> genMaybe genPrintableText <*> genPrintableUrl

instance ScrapboxSyntax LinkSpan where
    render (LinkSpan (Just name) url) = "[" <> name <> " " <> url <> "]"
    render (LinkSpan Nothing url)     = "[" <> url <> "]"
    getContent (LinkSpan mName url)   = fromMaybe mempty mName <> url

linkSpec :: Spec
linkSpec = describe "LINK" $ do
    prop "should parse link as LINK" $
        \(linkSpan :: LinkSpan) ->
            checkParsed linkSpan runSpanParser headMaybe isLink
    prop "should preserve its content" $
        \(linkSpan :: LinkSpan) -> checkContent linkSpan runSpanParser
            (\segments -> do
                guard $ length segments == 1
                segment <- headMaybe segments
                (LINK mName (Url url)) <- getLink segment
                return $ fromMaybe mempty mName <> url
            )
  where
    getLink :: Segment -> Maybe Segment
    getLink l@(LINK _ _) = Just l
    getLink _            = Nothing

--------------------------------------------------------------------------------
-- HashTag
--------------------------------------------------------------------------------

newtype HashTagSpan = HashTagSpan Text
    deriving Show

instance Arbitrary HashTagSpan where
    arbitrary = HashTagSpan <$> genText

instance ScrapboxSyntax HashTagSpan where
    render (HashTagSpan text)     = "#" <> text
    getContent (HashTagSpan text) = text

hashTagSpec :: Spec
hashTagSpec = describe "HASHTAG" $ do
    prop "should parse hashtag as HASHTAG" $
        \(hashTag :: HashTagSpan) ->
            checkParsed hashTag runSpanParser headMaybe isHashTag

    prop "should preserve its content" $
        \(hashTag :: HashTagSpan) ->
            checkContent hashTag runSpanParser
                (\segments -> do
                    guard $ length segments == 1
                    segment       <- headMaybe segments
                    (HASHTAG txt) <- getHashTag segment
                    return txt
                )
  where
    getHashTag :: Segment -> Maybe Segment
    getHashTag h@(HASHTAG _) = Just h
    getHashTag _             = Nothing
