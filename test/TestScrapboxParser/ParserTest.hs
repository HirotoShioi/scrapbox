{- Test suites for scrapbox parser
-}

module TestScrapboxParser.ParserTest
    ( parserSpec
    ) where

import           Test.Hspec (Spec)

import           TestScrapboxParser.Span (spanParserSpec)
import           TestScrapboxParser.Scrapbox (scrapboxParserSpec)
import           TestScrapboxParser.ScrapText (scrapTextParserSpec)

-- | Test specs for scrapbox parser
parserSpec :: Spec
parserSpec = do
    spanParserSpec
    scrapTextParserSpec
    scrapboxParserSpec
