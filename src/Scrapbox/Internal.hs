{-| Exposed modules used for testing
-}
module Scrapbox.Internal
    ( -- * Render functions
      renderBlock
    , renderSegments
    , renderText
    , renderInline
    -- * Parsers
    , runScrapTextParser
    , runItemParser
    , runScrapboxParser
    -- * Helper functions
    , concatInline
    , concatSegment
    , concatScrapText
    , verbose
    , unverbose
    , emptyStyle
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
    , isNoStyle
    ) where

import           Scrapbox.Parser.Scrapbox           (runScrapboxParser)
import           Scrapbox.Parser.Scrapbox.Item      (runItemParser)
import           Scrapbox.Parser.Scrapbox.ScrapText (runScrapTextParser)
import           Scrapbox.Render.Scrapbox           (renderBlock, renderInline,
                                                     renderSegments, renderText)
import           Scrapbox.Types                     (concatInline,
                                                     concatScrapText,
                                                     concatSegment, emptyStyle,
                                                     isBlockQuote, isBold,
                                                     isBulletPoint, isCodeBlock,
                                                     isCodeNotation, isHashTag,
                                                     isHeader, isItalic, isLink,
                                                     isMathExpr, isNoStyle,
                                                     isParagraph,
                                                     isStrikeThrough, isTable,
                                                     isText, isThumbnail,
                                                     unverbose, verbose)
