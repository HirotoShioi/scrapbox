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
    , shortListOf1
    ) where

import           RIO

import           Data.Scrapbox.Types (Block (..), InlineBlock (..),
                                      Segment (..), Style (..), concatInline,
                                      concatScrapText, concatSegment, unverbose,
                                      verbose)
import           Data.Scrapbox.Utils (genMaybe, genPrintableText,
                                      genPrintableUrl, shortListOf,
                                      shortListOf1)

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

-- | Checks if given 'Block' is an 'HEADING'
isHeader :: Block -> Bool
isHeader (HEADING _ _) = True
isHeader _             = False

-- | Checks whether given 'Block' is 'BLOCK_QUOTE'
isBlockQuote :: Block -> Bool
isBlockQuote (BLOCK_QUOTE _) = True
isBlockQuote _               = False

-- | Checks whether given 'Block' is 'BULLET_POINT'
isBulletPoint :: Block -> Bool
isBulletPoint (BULLET_POINT _ _) = True
isBulletPoint _                  = False

-- | Checks whether given 'Block' is 'CODE_BLOCK'
isCodeBlock :: Block -> Bool
isCodeBlock (CODE_BLOCK _ _) = True
isCodeBlock _                = False

-- | Checks whether given 'Block' is 'PARAGRAPH'
isParagraph :: Block -> Bool
isParagraph (PARAGRAPH _) = True
isParagraph _             = False

-- | Checks whether given 'Block' is 'THUMBNAIL'
isThumbnail :: Block -> Bool
isThumbnail (THUMBNAIL _) = True
isThumbnail _             = False

-- | Checks whether given 'Block' is 'TABLE'
isTable :: Block -> Bool
isTable (TABLE _ _) = True
isTable _           = False

-- | Checks whether given 'InlineBlock' is 'CODE_NOTATION'
isCodeNotation :: InlineBlock -> Bool
isCodeNotation (CODE_NOTATION _) = True
isCodeNotation _                 = False

-- | Checks whether given 'InlineBlock' is 'MATH_EXPRESSION'
isMathExpr :: InlineBlock -> Bool
isMathExpr (MATH_EXPRESSION _) = True
isMathExpr _                   = False

-- | Checks whether given 'Segment' is 'TEXT'
isText :: Segment -> Bool
isText (TEXT _) = True
isText _        = False

-- | Checks whether given 'Segment' is 'LINK'
isLink :: Segment -> Bool
isLink (LINK _ _) = True
isLink _          = False

-- | Checks whether given 'Segment' is 'HASHTAG'
isHashTag :: Segment -> Bool
isHashTag (HASHTAG _) = True
isHashTag _           = False

-- | Checks whether given 'Style' is 'Bold'
isBold :: Style -> Bool
isBold Bold = True
isBold _    = False

-- | Checks whether given 'Style' is 'Italic'
isItalic :: Style -> Bool
isItalic Italic = True
isItalic _      = False

-- | Checks whether given 'Style' is 'StrikeThrough'
isStrikeThrough :: Style -> Bool
isStrikeThrough StrikeThrough = True
isStrikeThrough _             = False

-- | Checks whether given 'Style' is 'Sized'
isSized :: Style -> Bool
isSized (Sized _) = True
isSized _         = False
