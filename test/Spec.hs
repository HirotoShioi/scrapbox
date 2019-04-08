{- Main function for exporting test suites
-}

module Main where

import           RIO

import           Test.Hspec (describe, hspec, parallel)

import           TestCommonMark.Commonmark (commonmarkSpec)
import           TestScrapboxParser.ParserTest (parserSpec)

main :: IO ()
main = hspec $ parallel $ do
    describe "Commonmark parser"
        commonmarkSpec

    describe "Scrapbox parser"
        parserSpec
