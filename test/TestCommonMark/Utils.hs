{-| Utility functions used to test Syntax parser
-}

module TestCommonMark.Utils
    ( checkScrapbox
    , getHeadSegment
    , getHeadInlineBlock
    , getParagraph
    ) where

import           RIO

import           RIO.List (headMaybe)

import           Data.Scrapbox (Block (..), InlineBlock (..), ScrapText (..),
                                Scrapbox (..), Segment, commonmarkToNode)
import           Test.QuickCheck (Property, Testable (..))
import           Utils (Syntax (..))

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

-- | General function used to test if given 'Syntax' can be properly parsed
-- and extract the expected element
checkScrapbox :: (Syntax a)
              => a
              -> (parsedContent -> Property)
              -> ([Block] -> Maybe parsedContent)
              -> Property
checkScrapbox markdown pro extractionFunc = do
    let (Scrapbox content) = parseMarkdown markdown
    property $ maybe (property False) pro (extractionFunc content)
  where
    -- | Parse given datatype into 'Scrapbox'
    parseMarkdown :: Syntax a => a -> Scrapbox
    parseMarkdown = commonmarkToNode [] . render

-- | Return 'PARAGRAPH' if given 'Block' is 'PARAGRAPH'
getParagraph :: Block -> Maybe Block
getParagraph paragraph@(PARAGRAPH _) = Just paragraph
getParagraph _                       = Nothing

-- | Extract heed segment of a given list of blocks
getHeadSegment :: [Block] -> Maybe Segment
getHeadSegment blocks = do
    blockContent                  <- headMaybe blocks
    PARAGRAPH (ScrapText inlines) <- getParagraph blockContent
    SPAN _ segments               <- headMaybe inlines
    headMaybe segments

-- | Extract heed segment of a given list of blocks
getHeadInlineBlock :: [Block] -> Maybe InlineBlock
getHeadInlineBlock blocks = do
    blockContent                  <- headMaybe blocks
    PARAGRAPH (ScrapText inlines) <- getParagraph blockContent
    headMaybe inlines
