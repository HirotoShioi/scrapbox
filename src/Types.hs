module Types where

import           RIO
import           RIO.Time

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
    = BreakLine
    -- ^ Simply breaks a line
    | BlockQuote ScrapText
    -- ^ BlockQuote like markdown
    | BulletLine BulletSize ScrapText
    -- ^ Bulletpoint styled line
    | BulletPoints [ScrapText]
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
    | PlainText Text
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
