{-| Test suites for commonmark parser
-}

module TestCommonMark.Commonmark where

import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           TestCommonMark.Blocks   (blockSpec)
import           TestCommonMark.Segments (segmentSpec)
import           TestCommonMark.Styles   (styleSpec)


commonmarkSpec :: Spec
commonmarkSpec = describe "CommonMark parser" $ modifyMaxSuccess (const 200) $ do
    blockSpec
    segmentSpec
    styleSpec
