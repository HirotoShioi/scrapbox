{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           RIO

import           Test.Hspec              (Spec, describe, hspec)
import           Test.Hspec.QuickCheck   (modifyMaxSuccess)

import           TestCommonMark.Segments (segmentSpec)
import           TestCommonMark.Styles   (styleSpec)
import           TestCommonMark.Blocks

main :: IO ()
main = hspec $ do
    commonMarkSpec

commonMarkSpec :: Spec
commonMarkSpec = describe "CommonMark parser" $ 
    modifyMaxSuccess (const 200) $ do
    blockSpec
    segmentSpec
    styleSpec