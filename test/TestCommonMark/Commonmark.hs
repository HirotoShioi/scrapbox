{-| Test suites for commonmark parser
-}

{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Commonmark
    ( commonmarkSpec
    ) where

import           RIO
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

import           TestCommonMark.Blocks (blockSpec)
import           TestCommonMark.Segments (segmentSpec)
import           TestCommonMark.Styles (styleSpec)

import           Data.Scrapbox.Internal (runParagraphParser)
import           Utils (propNonNull, shouldParseSpec)

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
        propNonNull runParagraphParser id
