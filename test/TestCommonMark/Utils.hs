{-| Utility functions used to test commonmark parser
-}

module TestCommonMark.Utils
    ( CommonMark(..)
    , checkScrapbox
    , getHeadSegment
    , getHeadInlineBlock
    , getParagraph
    ) where

import           RIO

import           RIO.List        (headMaybe)

import           Scrapbox        (Block (..), InlineBlock (..), ScrapText (..),
                                  Scrapbox (..), Segment, commonmarkToNode)
import           Test.QuickCheck (Property, Testable (..))

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

-- | Typeclass in which is used to render given datatype into markdown format.
class CommonMark a where
    render :: a -> Text

-- | General function used to test if given 'CommonMark' can be properly parsed
-- and extract the expected element
checkScrapbox :: (CommonMark a)
              => a
              -> (parsedContent -> Bool)
              -> ([Block] -> Maybe parsedContent)
              -> Property
checkScrapbox markdown pre extractionFunc = property $ do
    let (Scrapbox content) = parseMarkdown markdown
    maybe False pre (extractionFunc content)
  where
    -- | Parse given datatype into 'Scrapbox'
    parseMarkdown :: CommonMark a => a -> Scrapbox
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
    ITEM _ segments               <- headMaybe inlines
    headMaybe segments

-- | Extract heed segment of a given list of blocks
getHeadInlineBlock :: [Block] -> Maybe InlineBlock
getHeadInlineBlock blocks = do
    blockContent                  <- headMaybe blocks
    PARAGRAPH (ScrapText inlines) <- getParagraph blockContent
    headMaybe inlines
