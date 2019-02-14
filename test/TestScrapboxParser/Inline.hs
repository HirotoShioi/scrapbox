{-| Test suites for 'runItemParser'
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.Inline
    ( inlineParserSpec
    ) where

import           RIO                      hiding (assert)

import           Test.Hspec               (Spec, describe, it)
import           Test.Hspec.QuickCheck    (modifyMaxSuccess, prop)
import           Test.QuickCheck.Monadic  (assert, monadicIO)

import           Parser.Item              (runItemParser)
import           TestScrapboxParser.Utils (NonEmptyPrintableString (..),
                                           propParseAsExpected, shouldParseSpec)
import           Types                    (Segment (..), Url (..))
import           Utils                    (whenRight)

-- | Spec for inline text parser
inlineParserSpec :: Spec
inlineParserSpec =
    describe "Item parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runItemParser

        prop "should return non-empty list of segments if given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runItemParser $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \parsedContent ->
                    assert $ not $ null parsedContent

        it "should parse given text as expected" $
            propParseAsExpected exampleText expected runItemParser
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
