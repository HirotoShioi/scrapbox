{- Main function for exporting test suites
-}

module Main
  ( main,
  )
where

import RIO
import Test.Hspec (describe, hspec, parallel)
import TestCommonMark.Commonmark (commonmarkSpec)
import TestScrapboxParser.ParserTest (parserSpec)

main :: IO ()
main = hspec $ do
  describe "Scrapbox parser" $
    parallel
      parserSpec
  describe
    "Commonmark parser"
    commonmarkSpec
