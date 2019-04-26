{- Test suites for scrapbox parser
-}

module TestScrapboxParser.ParserTest
    ( parserSpec
    ) where

import           Test.Hspec (Spec)

import           TestScrapboxParser.Scrapbox (scrapboxParserSpec)
import           TestScrapboxParser.ScrapText (scrapTextParserSpec)
import           TestScrapboxParser.Span (spanParserSpec)

-- | Test specs for scrapbox parser
parserSpec :: Spec
parserSpec = do
    scrapboxParserSpec
    spanParserSpec
    scrapTextParserSpec
