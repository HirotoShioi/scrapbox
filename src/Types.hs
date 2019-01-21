{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Types where

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

-- | Markdown consist of list of 'Blocks'
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
    | BulletPoint !BulletSize !ScrapText
    -- ^ Bulletpoint styled line
    | BulletList ![Block]
    -- ^ Bullet points
    -- 'Block' for now, but it can be more type safe (although would become verbose)
    | CodeBlock !CodeName !CodeSnippet
    -- ^ Code blocks
    | Header !HeaderSize !Content
    -- ^ Header
    | Paragraph !ScrapText
    -- ^ Simple text
    | Table !TableName !TableContent -- No sure how to implement yet!!
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

type Content = [Segment]

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
    deriving (Eq, Show, Generic, Read, Ord)

-- | Segment
data Segment =
      CodeNotation !Text
    -- ^ CodeNotation
    | HashTag !Text
    -- ^ Hashtag
    | Link !(Maybe Text) !Url
    -- ^ Link, it can have href
    | SimpleText !Text
    -- ^ Just an simple text
    deriving (Eq, Show, Generic, Read, Ord)

-- | StyleData
data StyleData = StyleData
    { sHeaderSize    :: !Int
    -- ^ Size of an header,
    , sBold          :: !Bool
    -- ^ Bold style
    , sItalic        :: !Bool
    -- ^ Italic style
    , bStrikeThrough :: !Bool
    -- ^ Strike through
    } deriving (Eq, Show, Generic, Read, Ord)

--------------------------------------------------------------------------------
-- Verbose/Unverbose
--------------------------------------------------------------------------------

-- | Convert given Markdown into verbose structure
verbose :: Markdown -> Markdown
verbose (Markdown blocks) = Markdown $ map convertToVerbose blocks
  where
    convertToVerbose :: Block -> Block
    convertToVerbose = \case
        BlockQuote stext      -> BlockQuote $ verboseScrapText stext
        BulletList stexts     -> BulletList $ map convertToVerbose stexts
        BulletPoint num stext -> BulletPoint num (verboseScrapText stext)
        Paragraph stext       -> Paragraph $ verboseScrapText stext
        other                 -> other
    verboseScrapText :: ScrapText -> ScrapText
    verboseScrapText (ScrapText ctxs) = ScrapText $ concatMap mkVerboseContext ctxs
    mkVerboseContext :: Context -> [Context]
    mkVerboseContext (Context style segments) =
        foldr (\segment acc -> [Context style [segment]] <> acc) mempty segments

-- | Convert given Markdown into unverbose structure
unverbose :: Markdown -> Markdown
unverbose (Markdown blocks) = Markdown $ map unVerboseBlocks blocks
  where
    unVerboseBlocks :: Block -> Block
    unVerboseBlocks = \case
        BlockQuote stext      -> BlockQuote $ unVerboseScrapText stext
        BulletList stexts     -> BulletList $ map unVerboseBlocks stexts
        BulletPoint num stext -> BulletPoint num (unVerboseScrapText stext)
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

-- Concatenate 'ScrapText'
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

-- | Checks whether given 'Block' is 'CodeBlock'
isCodeBlock :: Block -> Bool
isCodeBlock (CodeBlock _ _) = True
isCodeBlock _               = False

-- | Checks whether given 'Block' is 'Paragraph'
isParagraph :: Block -> Bool
isParagraph (Paragraph _) = True
isParagraph _             = False

-- | Checks whether given 'Block' is 'BulletList'
isBulletList :: Block -> Bool
isBulletList (BulletList _) = True
isBulletList _              = False

-- | Checks whether given 'Block' is Thumbnail
isThumbnail :: Block -> Bool
isThumbnail (Thumbnail _) = True
isThumbnail _             = False

-- | Checks whether given 'Block' is Table
isTable :: Block -> Bool
isTable (Table _ _) = True
isTable _           = False

-- | Checks whether given Segment is Link
isLink :: Segment -> Bool
isLink (Link _ _) = True
isLink _          = False

-- | Checks whether given 'Segment is Code notation
isCodeNotation :: Segment -> Bool
isCodeNotation (CodeNotation _) = True
isCodeNotation _                = False

-- | Checks whether given 'Segment is Simple text
isSimpleText :: Segment -> Bool
isSimpleText (SimpleText _) = True
isSimpleText _              = False
