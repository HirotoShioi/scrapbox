{- Main function for exporting test suites
-}

module Main where

import           RIO

import           Test.Hspec                    (describe, hspec)

import           TestCommonMark.CommonMark     (commonmarkSpec)
import           TestScrapboxParser.ParserTest (parserSpec)

main :: IO ()
main = hspec $ do
    describe "Commonmark parser"
        commonmarkSpec

    describe "Scrapbox parser"
        parserSpec
