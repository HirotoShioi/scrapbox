{-| Datatypes used to represent the scrapbox parse tree as well as some of the helper functions.
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
    , Content
    , Context(..)
    , ScrapText(..)
    , Style(..)
    , StyleData(..)
    , TableContent(..)
    -- * Helper functions
    , concatContext
    , concatScrapText
    , verbose
    , unverbose
    , emptyContext
    , emptyStyle
    -- * Predicates
    , isBlockQuote
    , isBulletPoint
    , isCodeBlock
    , isCodeNotation
    , isHeader
    , isLink
    , isParagraph
    , isSimpleText
    , isThumbnail
    , isTable
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
    | HEADING !Level !Content
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

-- | ScrapText are consisted by list of 'Context'
newtype ScrapText = ScrapText [Context]
    deriving (Eq, Show, Generic, Read, Ord)

-- | Context is an content which are associated with an 'Style'
data Context = Context !Style !Content
    deriving (Eq, Show, Generic, Read, Ord)

-- | Content is list of 'Segment's
type Content = [Segment]

-- | Segment
data Segment =
      CODE_NOTATION !Text
    -- ^ CodeNotation
    | HASHTAG !Text
    -- ^ Hashtag
    | LINK !(Maybe Text) !Url
    -- ^ Link, it can have named as href
    | TEXT !Text
    -- ^ Just an simple text
    deriving (Eq, Show, Generic, Read, Ord)

-- | Style that can be applied to the 'Segment'
data Style =
      CustomStyle StyleData
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
    verboseScrapText (ScrapText ctxs) = ScrapText $ concatMap mkVerboseContext ctxs

    mkVerboseContext :: Context -> [Context]
    mkVerboseContext (Context style segments) =
        foldr (\segment acc -> [Context style [segment]] <> acc) mempty segments

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
    unVerboseScrapText (ScrapText ctxs) = ScrapText $ concatMap concatContext $ groupBy
        (\(Context style1 _) (Context style2 _) -> style1 == style2) ctxs

-- | Concatinate 'CONTEXT' with same style
concatContext :: [Context] -> [Context]
concatContext []       = []
concatContext [ctx]    = [ctx]
concatContext (c1@(Context style1 ctx1):c2@(Context style2 ctx2):rest)
    | style1 == style2 = concatContext (Context style1 (ctx1 <> ctx2) : rest)
    | otherwise        = c1 : c2 : concatContext rest

-- | Concatenate 'ScrapText'
-- This could be Semigroup, but definitely not Monoid (there's no mempty)
concatScrapText :: ScrapText -> ScrapText -> ScrapText
concatScrapText (ScrapText ctx1) (ScrapText ctx2) = ScrapText $ concatContext $ ctx1 <> ctx2

-- | Context with no content
emptyContext :: Context
emptyContext = Context NoStyle []

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

-- | Checks if given 'Block' is an 'HEADING'
isHeader :: Block -> Bool
isHeader (HEADING _ _) = True
isHeader _            = False

-- | Checks whether given 'Block' is 'BLOCK_QUOTE'
isBlockQuote :: Block -> Bool
isBlockQuote (BLOCK_QUOTE _) = True
isBlockQuote _              = False

-- | Checks whether given 'Block' is 'BULLET_POINT'
isBulletPoint :: Block -> Bool
isBulletPoint (BULLET_POINT _ _) = True
isBulletPoint _                 = False

-- | Checks whether given 'Block' is 'CODE_BLOCK'
isCodeBlock :: Block -> Bool
isCodeBlock (CODE_BLOCK _ _) = True
isCodeBlock _               = False

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

-- | Checks whether given 'Segment' is 'LINK'
isLink :: Segment -> Bool
isLink (LINK _ _) = True
isLink _          = False

-- | Checks whether given 'Segment' is 'CODE_NOTATION'
isCodeNotation :: Segment -> Bool
isCodeNotation (CODE_NOTATION _) = True
isCodeNotation _                = False

-- | Checks whether given 'Segment' is 'TEXT'
isSimpleText :: Segment -> Bool
isSimpleText (TEXT _) = True
isSimpleText _              = False
