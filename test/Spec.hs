import           RIO

import           Test.Hspec              (Spec, describe, hspec)
import           Test.Hspec.QuickCheck   (modifyMaxSuccess)

import           TestCommonMark.Segments (segmentSpec)
import           TestCommonMark.Styles   (styleSpec)
import           TestCommonMark.Blocks   (blockSpec)

main :: IO ()
main = hspec
    commonMarkSpec

commonMarkSpec :: Spec
commonMarkSpec = describe "CommonMark parser" $ modifyMaxSuccess (const 200) $ do
    blockSpec
    segmentSpec
    styleSpec