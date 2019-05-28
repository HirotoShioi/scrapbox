{-| Exposed modules used for testing
-}
module Data.Scrapbox.Internal
    ( -- * Render functions
      renderBlock
    , renderSegments
    , renderScrapText
    , renderInline
    , renderWithStyle
    -- * Commonmark
    , renderInlineBlock
    -- * Parsers
    -- These parsers can be used to parse given 'String' into some data type
    -- ** Scrapbox
    , runScrapTextParser
    , runSpanParser
    , runScrapboxParser
    -- ** Commonmark
    , runParagraphParser
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

import           Data.Scrapbox.Parser.Commonmark.ParagraphParser (runParagraphParser)
import           Data.Scrapbox.Parser.Scrapbox (runScrapboxParser)
import           Data.Scrapbox.Parser.Scrapbox.ScrapText (runScrapTextParser)
import           Data.Scrapbox.Parser.Scrapbox.Span (runSpanParser)
import           Data.Scrapbox.Render.Commonmark (renderInlineBlock)
import           Data.Scrapbox.Render.Scrapbox (renderBlock, renderInline,
                                                renderScrapText, renderSegments,
                                                renderWithStyle)
import           Data.Scrapbox.Types (concatInline, concatScrapText,
                                      concatSegment, isBlockQuote, isBold,
                                      isBulletPoint, isCodeBlock,
                                      isCodeNotation, isHashTag, isHeader,
                                      isItalic, isLink, isMathExpr, isParagraph,
                                      isSized, isStrikeThrough, isTable, isText,
                                      isThumbnail, unverbose, verbose)
import           Data.Scrapbox.Utils (genMaybe, genPrintableText,
                                      genPrintableUrl, shortListOf)
