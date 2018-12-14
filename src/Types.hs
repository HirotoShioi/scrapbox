{-# LANGUAGE LambdaCase #-}

module Types where

import           RIO
import           RIO.Time
import           Data.List (groupBy)

-- https://scrapbox.io/help/Syntax

-- TODO: Make naming more easy to understand/ makes sense

-- | Data structure of an scrapbox page in JSON format
data Page = Page
    { pContent   :: !Markdown
    , pCreatedAt :: !UTCTime
    , pUpdatedAt :: !UTCTime
    , pTitle     :: !Text
    } deriving (Eq, Show)

newtype TableContent = TableContent {
    getTableContent :: [[Text]]
    } deriving (Eq, Show)

newtype Url = Url Text
    deriving (Eq, Show)

newtype CodeName = CodeName {
    getCodeName :: Text
    } deriving (Eq, Show)

newtype CodeSnippet = CodeSnippet {
    getCodeSnippet :: Text
    } deriving (Eq, Show)

-- | Markdown consist of list of 'Blocks'
newtype Markdown = Markdown [Block]
    deriving (Eq, Show)

newtype HeaderSize = HeaderSize Int
    deriving (Eq, Show)

newtype BulletSize = BulletSize Int
    deriving (Eq, Show)

-- | Blocks are contents
data Block
    = LineBreak
    -- ^ Simply breaks a line
    | BlockQuote ScrapText
    -- ^ BlockQuote like markdown
    | BulletPoint BulletSize ScrapText
    -- ^ Bulletpoint styled line
    | BulletList [ScrapText]
    -- ^ Bullet points
    | CodeBlock CodeName CodeSnippet
    -- ^ Code blocks
    | Header HeaderSize Content
    -- ^ Header
    | Document ScrapText
    -- ^ Simple text
    | Table TableContent -- No sure how to implement yet!!
    -- ^ Table
    | Thumbnail Url
    -- ^ Thumbnail
    deriving (Eq, Show)

-- | ScrapText are consisted by list of 'Context'
newtype ScrapText = ScrapText [Context]
    deriving (Eq, Show)

-- | Context is an content which are associated with an 'Style'
data Context = Context !Style !Content
    deriving (Eq, Show)

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
    deriving (Eq, Show)

-- | Segment
data Segment =
      CodeNotation Text
    -- ^ CodeNotation
    | Link (Maybe Text) Url
    -- ^ Link, it can have href
    | SimpleText Text
    -- ^ Just an simple text
    deriving (Eq, Show)

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
    } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Verbose/Unverbose
--------------------------------------------------------------------------------

-- | Convert given Markdown into verbose structure
mkVerbose :: Markdown -> Markdown
mkVerbose (Markdown blocks) = Markdown $ map convertToVerbose blocks
  where
    convertToVerbose :: Block -> Block
    convertToVerbose = \case
        BlockQuote stext      -> BlockQuote $ verboseScrapText stext
        BulletList stexts     -> BulletList $ map verboseScrapText stexts
        BulletPoint num stext -> BulletPoint num (verboseScrapText stext)
        Document stext        -> Document $ verboseScrapText stext
        other                 -> other
    verboseScrapText :: ScrapText -> ScrapText
    verboseScrapText (ScrapText ctxs) = ScrapText $ concatMap mkVerboseContext ctxs
    mkVerboseContext :: Context -> [Context]
    mkVerboseContext (Context style segments) = 
        foldr (\segment acc -> [Context style [segment]] <> acc) mempty segments

-- | Convert given Markdown into unverbose structure
unVerbose :: Markdown -> Markdown
unVerbose (Markdown blocks) = Markdown $ map unVerboseBlocks blocks
  where
    unVerboseBlocks :: Block -> Block
    unVerboseBlocks = \case
        BlockQuote stext      -> BlockQuote $ unVerboseScrapText stext
        BulletList stexts     -> BulletList $ map unVerboseScrapText stexts
        BulletPoint num stext -> BulletPoint num (unVerboseScrapText stext)
        Document stext        -> Document $ unVerboseScrapText stext
        other                 -> other
    unVerboseScrapText :: ScrapText -> ScrapText
    unVerboseScrapText (ScrapText ctxs) = ScrapText $ map concatContext $ groupBy 
        (\(Context style1 _) (Context style2 _) -> style1 == style2) ctxs
    concatContext :: [Context] -> Context
    concatContext [] = emptyContext
    concatContext ctxs@((Context style _):_) = 
        foldr (\(Context _ segments) (Context _ acc) -> Context style (segments <> acc)) emptyContext ctxs

emptyContext :: Context
emptyContext = Context NoStyle []