{- Test suites for scrapbox parser
-}

{-# LANGUAGE ScopedTypeVariables #-}

module ParserTest where

import           RIO                     hiding (assert)

import           Test.Hspec              (Spec, describe)
import           Test.Hspec.QuickCheck   (modifyMaxSuccess, prop)
import           Test.QuickCheck         (Arbitrary (..), PrintableString (..),
                                          arbitraryPrintableChar, listOf1)
import           Test.QuickCheck.Monadic (assert, monadicIO)

import           Parser.Inline           (runInlineParser)
import           Utils                   (whenRight)

-- | Test spec for inline parser
parserSpec :: Spec
parserSpec =
    describe "inline parser" $ modifyMaxSuccess (const 10000) $ do
        prop "should be able to parse any text without failing or cause infinite loop" $
            \(someText :: PrintableString) ->
                isRight $ runInlineParser $ getPrintableString someText

        prop "should not return empty list if given string is not empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runInlineParser $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \parsedContent ->
                    assert $ not $ null parsedContent

newtype NonEmptyPrintableString =  NonEmptyPrintableString {
    getNonEmptyPrintableString :: String
    } deriving Show

instance Arbitrary NonEmptyPrintableString where
    arbitrary = NonEmptyPrintableString <$> listOf1 arbitraryPrintableChar
