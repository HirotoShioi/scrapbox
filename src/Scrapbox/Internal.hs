{-| Exposed modules used for testing
-}
module Scrapbox.Internal
    ( -- Render functions
      renderBlock
    , renderSegments
    , renderText
    , renderInline
    -- Parsers
    , runScrapTextParser
    , runItemParser
    -- * Helper functions
    , concatInline
    , concatSegment
    , concatScrapText
    , verbose
    , unverbose
    -- * Predicates
    , isBlockQuote
    , isBulletPoint
    , isCodeBlock
    , isCodeNotation
    , isMathExpr
    , isHeader
    , isLink
    , isParagraph
    , isThumbnail
    , isTable
    , isText
    , isHashTag
    , isBold
    , isItalic
    , isStrikeThrough
    -- * For testing
    , findDiffs
    ) where

import           Scrapbox.DiffFinder       (findDiffs)
import           Scrapbox.Parser.Item      (runItemParser)
import           Scrapbox.Parser.ScrapText (runScrapTextParser)
import           Scrapbox.Render           (renderBlock, renderInline,
                                            renderSegments, renderText)
import           Scrapbox.Types            (concatInline, concatScrapText,
                                            concatSegment, isBlockQuote, isBold,
                                            isBulletPoint, isCodeBlock,
                                            isCodeNotation, isHashTag, isHeader,
                                            isItalic, isLink, isMathExpr,
                                            isParagraph, isStrikeThrough,
                                            isTable, isText, isThumbnail,
                                            unverbose, verbose)
