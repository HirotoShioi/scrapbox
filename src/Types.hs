{-| Datatypes used to represent the scrapbox parse tree as well as some of the helper functions.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Types
    ( -- * Datatypes
      Page (..)
    , Markdown (..)
    , BulletSize(..)
    , Block(..)
    , CodeName(..)
    , CodeSnippet(..)
    , HeaderSize(..)
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
    { pContent :: !Markdown
    -- ^ Content of the page
    , pTitle   :: !Text
    -- ^ Title
    } deriving (Eq, Show, Generic, Read, Ord)

-- | Markdown consist of list of 'Block's
newtype Markdown = Markdown [Block]
    deriving (Eq, Show, Generic, Read, Ord)

--------------------------------------------------------------------------------
-- Elements that are used in Block
--------------------------------------------------------------------------------

-- | Size of bullet point
newtype BulletSize = BulletSize Int
    deriving (Eq, Show, Generic, Read, Ord)

-- | Name of the code block
newtype CodeName = CodeName Text
    deriving (Eq, Show, Generic, Read, Ord)

-- | Code snippet
newtype CodeSnippet = CodeSnippet Text
    deriving (Eq, Show, Generic, Read, Ord)

-- | Header size
newtype HeaderSize = HeaderSize Int
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
    = LineBreak
    -- ^ Simply breaks a line
    | BlockQuote !ScrapText
    -- ^ BlockQuote like markdown
    | BulletPoint !BulletSize ![Block]
    -- ^ Bulletpoint styled line
    | CodeBlock !CodeName !CodeSnippet
    -- ^ Code blocks
    | Header !HeaderSize !Content
    -- ^ Header
    | Paragraph !ScrapText
    -- ^ Paragraph
    | Table !TableName !TableContent
    -- ^ Table
    | Thumbnail !Url
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
      CodeNotation !Text
    -- ^ CodeNotation
    | HashTag !Text
    -- ^ Hashtag
    | Link !(Maybe Text) !Url
    -- ^ Link, it can have named as href
    | SimpleText !Text
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

-- | Convert given Markdown into verbose structure
verbose :: Markdown -> Markdown
verbose (Markdown blocks) = Markdown $ map convertToVerbose blocks
  where
    convertToVerbose :: Block -> Block
    convertToVerbose = \case
        BlockQuote stext      -> BlockQuote $ verboseScrapText stext
        BulletPoint num block -> BulletPoint num $ map convertToVerbose block
        Paragraph stext       -> Paragraph $ verboseScrapText stext
        other                 -> other

    verboseScrapText :: ScrapText -> ScrapText
    verboseScrapText (ScrapText ctxs) = ScrapText $ concatMap mkVerboseContext ctxs

    mkVerboseContext :: Context -> [Context]
    mkVerboseContext (Context style segments) =
        foldr (\segment acc -> [Context style [segment]] <> acc) mempty segments

-- | Convert given Markdown into unverbose structure
unverbose :: Markdown -> Markdown
unverbose (Markdown blocks) = Markdown $ map unVerboseBlock blocks
  where
    unVerboseBlock :: Block -> Block
    unVerboseBlock = \case
        BlockQuote stext      -> BlockQuote $ unVerboseScrapText stext
        BulletPoint num block -> BulletPoint num $ map unVerboseBlock block
        Paragraph stext       -> Paragraph $ unVerboseScrapText stext
        other                 -> other

    unVerboseScrapText :: ScrapText -> ScrapText
    unVerboseScrapText (ScrapText ctxs) = ScrapText $ concatMap concatContext $ groupBy
        (\(Context style1 _) (Context style2 _) -> style1 == style2) ctxs

-- | Concatinate given contexts which has same style
concatContext :: [Context] -> [Context]
concatContext [] = []
concatContext [ctx] = [ctx]
concatContext (c1@(Context style1 ctx1):c2@(Context style2 ctx2):rest)
    | style1 == style2 = concatContext (Context style1 (ctx1 <> ctx2) : rest)
    | otherwise        = c1 : c2 : concatContext rest

-- | Concatenate 'ScrapText'
-- This could be Semigroup, but definatly not Monoid (there's no mempty)
concatScrapText :: ScrapText -> ScrapText -> ScrapText
concatScrapText (ScrapText ctx1) (ScrapText ctx2) = ScrapText $ concatContext $ ctx1 <> ctx2

-- | Context with no content
emptyContext :: Context
emptyContext = Context NoStyle []

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

-- | Checks if given 'Block' is an 'Header'
isHeader :: Block -> Bool
isHeader (Header _ _) = True
isHeader _            = False

-- | Checks whether given 'Block' is 'BlockQuote'
isBlockQuote :: Block -> Bool
isBlockQuote (BlockQuote _) = True
isBlockQuote _              = False

-- | Checks whether given 'Block' is 'BlockQuote'
isBulletPoint :: Block -> Bool
isBulletPoint (BulletPoint _ _) = True
isBulletPoint _                 = False

-- | Checks whether given 'Block' is 'CodeBlock'
isCodeBlock :: Block -> Bool
isCodeBlock (CodeBlock _ _) = True
isCodeBlock _               = False

-- | Checks whether given 'Block' is 'Paragraph'
isParagraph :: Block -> Bool
isParagraph (Paragraph _) = True
isParagraph _             = False

-- | Checks whether given 'Block' is 'Thumbnail'
isThumbnail :: Block -> Bool
isThumbnail (Thumbnail _) = True
isThumbnail _             = False

-- | Checks whether given 'Block' is 'Table'
isTable :: Block -> Bool
isTable (Table _ _) = True
isTable _           = False

-- | Checks whether given 'Segment' is 'Link'
isLink :: Segment -> Bool
isLink (Link _ _) = True
isLink _          = False

-- | Checks whether given 'Segment' is 'CodeNotation'
isCodeNotation :: Segment -> Bool
isCodeNotation (CodeNotation _) = True
isCodeNotation _                = False

-- | Checks whether given 'Segment' is 'SimpleText'
isSimpleText :: Segment -> Bool
isSimpleText (SimpleText _) = True
isSimpleText _              = False
