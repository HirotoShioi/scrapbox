{-| Test suites for commonmark parser
-}

{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Commonmark
    ( commonmarkSpec
    ) where

import           RIO hiding (assert)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck.Monadic (assert, monadicIO)

import           TestCommonMark.Blocks (blockSpec)
import           TestCommonMark.Segments (segmentSpec)
import           TestCommonMark.Styles (styleSpec)

import           Data.Scrapbox.Internal (runParagraphParser)
import           Utils (NonEmptyPrintableString (..), shouldParseSpec,
                        whenRight)

commonmarkSpec :: Spec
commonmarkSpec = describe "CommonMark parser" $ modifyMaxSuccess (const 200) $ do
    blockSpec
    segmentSpec
    styleSpec
    paragraphParserSpec

-- | Test case on @runParagraphParser@
paragraphParserSpec :: Spec
paragraphParserSpec = describe "runParagraphParser" $ modifyMaxSuccess (const 5000) $ do
    shouldParseSpec runParagraphParser

    prop "should return non-empty list of blocks if the given string is non-empty" $
        \(someText :: NonEmptyPrintableString) -> monadicIO $ do
            let eParseredText = runParagraphParser $ getNonEmptyPrintableString someText
            assert $ isRight eParseredText
            whenRight eParseredText $ \inlines ->
                assert $ not $ null inlines
