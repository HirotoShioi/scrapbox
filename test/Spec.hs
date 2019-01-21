import           RIO

import           Test.Hspec              (Spec, describe, hspec)
import           Test.Hspec.QuickCheck   (modifyMaxSuccess)

import           TestCommonMark.Blocks   (blockSpec)
import           TestCommonMark.Segments (segmentSpec)
import           TestCommonMark.Styles   (styleSpec)

import           ParserTest              (parserSpec)

main :: IO ()
main = hspec
    commonMarkSpec

commonMarkSpec :: Spec
commonMarkSpec = do
    describe "CommonMark parser" $ modifyMaxSuccess (const 200) $ do
        blockSpec
        segmentSpec
        styleSpec

    describe "Parser spec"
        parserSpec
