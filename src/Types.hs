{-| Datatypes used to represent the scrapbox AST as well as some of the helper functions.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Types
    ( -- * Datatypes
      Page (..)
    , Scrapbox (..)
    , Start(..)
    , Block(..)
    , CodeName(..)
    , CodeSnippet(..)
    , Level(..)
    , TableName(..)
    , Url(..)
    , Segment(..)
    , InlineBlock(..)
    , ScrapText(..)
    , Style(..)
    , StyleData(..)
    , TableContent(..)
    -- * Helper functions
    , concatInline
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
    ) where

import           RIO

import           Data.List (groupBy)

-- https://scrapbox.io/help/Syntax

-- TODO: Make naming more easy to understand/ makes sense

-- | Data structure of an scrapbox page in JSON format
data Page = Page
    { pContent :: !Scrapbox
    -- ^ Content of the page
    , pTitle   :: !Text
    -- ^ Title
    } deriving (Eq, Show, Generic, Read, Ord)

-- | Scrapbox page consist of list of 'Block'
newtype Scrapbox = Scrapbox [Block]
    deriving (Eq, Show, Generic, Read, Ord)

--------------------------------------------------------------------------------
-- Elements that are used in Block
--------------------------------------------------------------------------------

-- | Starting point of 'BULLET_POINT'
newtype Start = Start Int
    deriving (Eq, Show, Generic, Read, Ord)

-- | Name of the code block
newtype CodeName = CodeName Text
    deriving (Eq, Show, Generic, Read, Ord)

-- | Code snippet
newtype CodeSnippet = CodeSnippet Text
    deriving (Eq, Show, Generic, Read, Ord)

-- | Heading level
newtype Level = Level Int
    deriving (Eq, Show, Generic, Read, Ord)

-- | Name of the table
newtype TableName = TableName Text
    deriving (Eq, Show, Generic, Read, Ord)

-- | Content of the table
newtype TableContent = TableContent [[Text]]
    deriving (Eq, Show, Generic, Read, Ord)

-- | Url for Link/Thumbnail
newtype Url = Url Text
    deriving (Eq, Show, Generic, Read, Ord)

-- | Blocks are contents
data Block
    = LINEBREAK
    -- ^ Linebreak
    | BLOCK_QUOTE !ScrapText
    -- ^ BlockQuote
    | BULLET_POINT !Start ![Block]
    -- ^ Bulletpoint styled line
    | CODE_BLOCK !CodeName !CodeSnippet
    -- ^ Code blocks
    | HEADING !Level ![Segment]
    -- ^ Header
    | PARAGRAPH !ScrapText
    -- ^ Paragraph
    | TABLE !TableName !TableContent
    -- ^ Table
    | THUMBNAIL !Url
    -- ^ Thumbnail
    deriving (Eq, Show, Generic, Read, Ord)

--------------------------------------------------------------------------------
-- ScrapText
--------------------------------------------------------------------------------

-- | ScrapText are consisted by list of 'InlineBlock'
newtype ScrapText = ScrapText [InlineBlock]
    deriving (Eq, Show, Generic, Read, Ord)

-- | InlineBlock
data InlineBlock
    = ITEM !Style ![Segment]
    -- ^ ITEM are blocks which can have styles
    | CODE_NOTATION !Text
    -- ^ Code notation
    | MATH_EXPRESSION !Text
    deriving (Eq, Show, Generic, Read, Ord)

-- | Segment
data Segment
    = HASHTAG !Text
    -- ^ Hashtag
    | LINK !(Maybe Text) !Url
    -- ^ Link, it can have named as href
    | TEXT !Text
    -- ^ Just an simple text
    deriving (Eq, Show, Generic, Read, Ord)

-- | Style that can be applied to the 'Segment'
data Style
    = CustomStyle StyleData
    -- ^ You can use this to combine all three as of the styles as well as Header
    | Bold
    -- ^ Bold style
    | Italic
    -- ^ Italic style
    | NoStyle
    -- ^ No styles
    | StrikeThrough
    -- ^ StrikeThrough style
    | UserStyle Text
    deriving (Eq, Show, Generic, Read, Ord)

-- | StyleData
data StyleData = StyleData
    { sHeaderSize    :: !Int
    -- ^ Size of an header,
    , sBold          :: !Bool
    -- ^ Bold style
    , sItalic        :: !Bool
    -- ^ Italic style
    , sStrikeThrough :: !Bool
    -- ^ Strike through
    } deriving (Eq, Show, Generic, Read, Ord)

--------------------------------------------------------------------------------
-- Verbose/Unverbose
--------------------------------------------------------------------------------

-- | Empty style data
emptyStyle :: StyleData
emptyStyle = StyleData 0 False False False

-- | Convert given 'Scrapbox' into verbose structure
verbose :: Scrapbox -> Scrapbox
verbose (Scrapbox blocks) = Scrapbox $ map convertToVerbose blocks
  where
    convertToVerbose :: Block -> Block
    convertToVerbose = \case
        BLOCK_QUOTE stext      -> BLOCK_QUOTE $ verboseScrapText stext
        BULLET_POINT num block -> BULLET_POINT num $ map convertToVerbose block
        PARAGRAPH stext        -> PARAGRAPH $ verboseScrapText stext
        other                  -> other

    verboseScrapText :: ScrapText -> ScrapText
    verboseScrapText (ScrapText inlines) = ScrapText $ concatMap mkVerboseInlineBlock inlines

    mkVerboseInlineBlock :: InlineBlock -> [InlineBlock]
    mkVerboseInlineBlock (ITEM style segments) =
        foldr (\segment acc -> [ITEM style [segment]] <> acc) mempty segments
    mkVerboseInlineBlock others                = [others]

-- | Convert given 'Scrapbox' into unverbose structure
unverbose :: Scrapbox -> Scrapbox
unverbose (Scrapbox blocks) = Scrapbox $ map unVerboseBlock blocks
  where
    unVerboseBlock :: Block -> Block
    unVerboseBlock = \case
        BLOCK_QUOTE stext      -> BLOCK_QUOTE $ unVerboseScrapText stext
        BULLET_POINT num block -> BULLET_POINT num $ map unVerboseBlock block
        PARAGRAPH stext        -> PARAGRAPH $ unVerboseScrapText stext
        other                  -> other

    unVerboseScrapText :: ScrapText -> ScrapText
    unVerboseScrapText (ScrapText inlines) = ScrapText $ concatMap concatInline $ groupBy isSameStyle inlines

    isSameStyle :: InlineBlock -> InlineBlock -> Bool
    isSameStyle (ITEM style1 _) (ITEM style2 _) = style1 == style2
    isSameStyle _ _                             = False

-- | Concatinate 'ITEM' with same style
concatInline :: [InlineBlock] -> [InlineBlock]
concatInline []       = []
concatInline [inline] = [inline]
concatInline (c1@(ITEM style1 inline1):c2@(ITEM style2 inline2):rest)
    | style1 == style2 = concatInline (ITEM style1 (inline1 <> inline2) : rest)
    | otherwise        = c1 : concatInline (c2:rest)
concatInline (a : c@(ITEM _ _) : rest) = a : concatInline (c:rest)
concatInline (a : b : rest)            = a : b : concatInline rest

-- | Concatenate 'ScrapText'
-- This could be Semigroup, but definitely not Monoid (there's no mempty)
concatScrapText :: ScrapText -> ScrapText -> ScrapText
concatScrapText (ScrapText inline1) (ScrapText inline2) = ScrapText $ concatInline $ inline1 <> inline2

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

-- | Checks whether given 'Inline' is 'MATH_EXPRESSION'
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
