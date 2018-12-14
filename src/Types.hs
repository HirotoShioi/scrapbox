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
-- Smart constructors
--------------------------------------------------------------------------------

textBlock :: Text -> Block
textBlock text = Document $ ScrapText [noStyle [simpleText text]]

bulletPoint :: Int -> Text -> Block
bulletPoint num text = BulletPoint (BulletSize num) $ ScrapText $ [noStyle [simpleText text]]

lineBreak :: Block
lineBreak = LineBreak

simpleText :: Text -> Segment
simpleText = SimpleText

codeNotation :: Text -> Segment
codeNotation = CodeNotation

link :: Maybe Text -> Url -> Segment
link = Link

noStyle :: [Segment] -> Context
noStyle segments = Context NoStyle segments

bold :: [Segment] -> Context
bold segments = Context Bold segments

italic :: [Segment] -> Context
italic segments = Context Italic segments

strikeThrough :: [Segment] -> Context
strikeThrough segments = Context StrikeThrough segments

-- | Smart constructor for creating 'Markdown' with given '[Block]'
mkMarkdown :: [Block] -> Markdown
mkMarkdown = Markdown

-- | Convert given Markdown into verbose structure
mkVerbose :: Markdown -> Markdown
mkVerbose (Markdown blocks) = Markdown $ map convertToVerbose blocks
  where
    convertToVerbose :: Block -> Block
    convertToVerbose = \case
        BlockQuote scrapText      -> BlockQuote $ verboseScrapText scrapText
        BulletList scrapTexts     -> BulletList $ map verboseScrapText scrapTexts
        BulletPoint num scrapText -> BulletPoint num (verboseScrapText scrapText)
        Document scrapText        -> Document $ verboseScrapText scrapText
        other                     -> other
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
        BlockQuote scrapText      -> BlockQuote $ unVerboseScrapText scrapText
        BulletList scrapTexts     -> BulletList $ map unVerboseScrapText scrapTexts
        BulletPoint num scrapText -> BulletPoint num (unVerboseScrapText scrapText)
        Document scrapText        -> Document $ unVerboseScrapText scrapText
        other                     -> other
    unVerboseScrapText :: ScrapText -> ScrapText
    unVerboseScrapText (ScrapText ctxs) = ScrapText $ map concatContext $ groupBy 
        (\(Context style1 _) (Context style2 _) -> style1 == style2) ctxs
    concatContext :: [Context] -> Context
    concatContext [] = emptyContext
    concatContext ctxs@((Context style _):_) = 
        foldr (\(Context _ segments) (Context _ acc) -> Context style (segments <> acc)) emptyContext ctxs

emptyContext :: Context
emptyContext = Context NoStyle []