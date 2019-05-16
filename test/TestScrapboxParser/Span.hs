{-| Test suites for 'runSpanParser'
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.Span
    ( spanParserSpec
    ) where

import           RIO

import           RIO.List (headMaybe)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), (===), property)

import           Data.Scrapbox (Segment (..), Url (..))
import           Data.Scrapbox.Internal (genPrintableText, isHashTag, isLink,
                                         isText, runSpanParser)
import           TestScrapboxParser.Utils (Syntax (..),
                                           checkParsed, propParseAsExpected)
import           Utils (genMaybe, genPrintableUrl, genText, propNonNull,
                        shouldParseSpec)

-- TODO: Replace genText with genPrintableText

-- | Spec for inline text parser
spanParserSpec :: Spec
spanParserSpec =
    describe "Span parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runSpanParser

        prop "should return non-empty list of segments if given string is non-empty" $
            propNonNull runSpanParser id

        it "should parse given text as expected" $
            propParseAsExpected exampleText expected runSpanParser

        -- Span specs
        describe "Spans" $ modifyMaxSuccess (const 10000) $ do
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
    arbitrary = TextSpan <$> genText

instance Syntax TextSpan where
    render (TextSpan txt)     = txt


--- Text
textSpec :: Spec
textSpec = describe "TEXT" $ do
    prop "should parse text as TEXT" $
        \(someText :: TextSpan) ->
            checkParsed someText runSpanParser headMaybe (property . isText)
    prop "should preserve its content" $
        \(someText@(TextSpan txt)) ->
            checkParsed someText runSpanParser
                (\segments -> do
                  guard $ length segments == 1
                  headMaybe segments
                )
                (=== TEXT txt)

--------------------------------------------------------------------------------
-- Link
--------------------------------------------------------------------------------

data LinkSpan = LinkSpan !(Maybe Text) !Text
    deriving Show

instance Arbitrary LinkSpan where
    arbitrary = LinkSpan <$> genMaybe genPrintableText <*> genPrintableUrl

instance Syntax LinkSpan where
    render (LinkSpan (Just name) url) = "[" <> name <> " " <> url <> "]"
    render (LinkSpan Nothing url)     = "[" <> url <> "]"

linkSpec :: Spec
linkSpec = describe "LINK" $ do
    prop "should parse link as LINK" $
        \(linkSpan :: LinkSpan) ->
            checkParsed linkSpan runSpanParser headMaybe (property . isLink)
    prop "should preserve its content" $
        \(linkSpan@(LinkSpan mName url)) -> 
            checkParsed
                linkSpan
                runSpanParser
                (\segments -> do
                    guard $ length segments == 1
                    headMaybe segments
                )
                (=== LINK mName (Url url))

--------------------------------------------------------------------------------
-- HashTag
--------------------------------------------------------------------------------

newtype HashTagSpan = HashTagSpan Text
    deriving Show

instance Arbitrary HashTagSpan where
    arbitrary = HashTagSpan <$> genText

instance Syntax HashTagSpan where
    render (HashTagSpan text)     = "#" <> text

hashTagSpec :: Spec
hashTagSpec = describe "HASHTAG" $ do
    prop "should parse hashtag as HASHTAG" $
        \(hashTag :: HashTagSpan) ->
            checkParsed hashTag runSpanParser headMaybe (property . isHashTag)

    prop "should preserve its content" $
        \(hashTag@(HashTagSpan tag)) ->
            checkParsed hashTag runSpanParser
                (\segments -> do
                    guard $ length segments == 1
                    headMaybe segments
                )
                (=== HASHTAG tag)
