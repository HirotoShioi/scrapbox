{- Test suites for scrapbox parser
-}

module TestScrapboxParser.ParserTest
    ( parserSpec
    ) where

import           Test.Hspec (Spec)

import           TestScrapboxParser.Inline (inlineParserSpec)
import           TestScrapboxParser.Scrapbox (scrapboxParserSpec)
import           TestScrapboxParser.ScrapText (scrapTextParserSpec)

-- | Test specs for scrapbox parser
parserSpec :: Spec
parserSpec = do
    inlineParserSpec
    scrapTextParserSpec
    scrapboxParserSpec
