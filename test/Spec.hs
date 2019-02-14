{- Main function for exporting test suites
-}

module Main where

import           RIO

import           Test.Hspec                    (describe, hspec)
import           Test.Hspec.QuickCheck         (modifyMaxSuccess)

import           TestCommonMark.Blocks         (blockSpec)
import           TestCommonMark.Segments       (segmentSpec)
import           TestCommonMark.Styles         (styleSpec)
import           TestScrapboxParser.ParserTest (parserSpec)

main :: IO ()
main = hspec $ do
    describe "CommonMark parser" $ modifyMaxSuccess (const 200) $ do
        blockSpec
        segmentSpec
        styleSpec

    describe "Parser spec"
        parserSpec
