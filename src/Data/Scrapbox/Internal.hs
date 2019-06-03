{-| Exposed modules used for testing
-}
module Data.Scrapbox.Internal
    (
    -- * Helper functions
      concatInline
    , concatSegment
    , concatScrapText
    , verbose
    , unverbose
    -- * Predicates
    , isBlockQuote
    , isBulletPoint
    , isCodeBlock
    , isHeader
    , isLink
    , isParagraph
    , isThumbnail
    , isTable
    , isText
    , isMathExpr
    , isCodeNotation
    , isHashTag
    , isBold
    , isItalic
    , isStrikeThrough
    , isSized
    -- * For testing
    , genPrintableText
    , genPrintableUrl
    , genMaybe
    , shortListOf
    ) where

import           Data.Scrapbox.Types (concatInline, concatScrapText,
                                      concatSegment, isBlockQuote, isBold,
                                      isBulletPoint, isCodeBlock,
                                      isCodeNotation, isHashTag, isHeader,
                                      isItalic, isLink, isMathExpr, isParagraph,
                                      isSized, isStrikeThrough, isTable, isText,
                                      isThumbnail, unverbose, verbose)
import           Data.Scrapbox.Utils (genMaybe, genPrintableText,
                                      genPrintableUrl, shortListOf)
