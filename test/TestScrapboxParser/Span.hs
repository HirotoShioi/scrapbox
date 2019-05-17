{-| Test suites for 'runSpanParser'
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.Span where

import           RIO

import           RIO.List (headMaybe)
import qualified RIO.Text as T
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, property, (===))

import           Data.Scrapbox (Segment (..), Url (..))
import           Data.Scrapbox.Internal (renderSegments, runSpanParser)
import           TestScrapboxParser.Utils (propParseAsExpected)
import           Utils (propNonNull, shouldParseSpec)

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
        describe "Spans" $ modifyMaxSuccess (const 10000) $ 
            prop "round trip tests" segmentRoundTripTest
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
-- Model test
--------------------------------------------------------------------------------

segmentRoundTripTest :: Segment -> Property
segmentRoundTripTest segment =
    let rendered = renderSegments [segment]
    in either
        (const $ property  False)
        checkContent
        (runSpanParser $ T.unpack rendered)
  where
    checkContent = maybe (property False) (=== segment) . headMaybe