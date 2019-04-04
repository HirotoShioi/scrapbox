{-| Exposed modules used for testing
-}
module Data.Scrapbox.Internal
    ( -- * Render functions
      renderBlock
    , renderSegments
    , renderText
    , renderInline
    -- * Parsers
    , runScrapTextParser
    , runItemParser
    , runScrapboxParser
    , runParagraphParser
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

import           Data.Scrapbox.Parser.Commonmark.ParagraphParser (runParagraphParser)
import           Data.Scrapbox.Parser.Scrapbox (runScrapboxParser)
import           Data.Scrapbox.Parser.Scrapbox.Item (runItemParser)
import           Data.Scrapbox.Parser.Scrapbox.ScrapText (runScrapTextParser)
import           Data.Scrapbox.Render.Scrapbox (renderBlock, renderInline,
                                                renderSegments, renderText)
import           Data.Scrapbox.Types (concatInline, concatScrapText,
                                      concatSegment, emptyStyle, isBlockQuote,
                                      isBold, isBulletPoint, isCodeBlock,
                                      isCodeNotation, isHashTag, isHeader,
                                      isItalic, isLink, isMathExpr, isNoStyle,
                                      isParagraph, isStrikeThrough, isTable,
                                      isText, isThumbnail, unverbose, verbose)
