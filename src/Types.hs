module Types where

import RIO
import RIO.Time

-- https://scrapbox.io/help/Syntax

-- TODO: Make naming more easy to understand/ makes sense

data Page = Page
    { pContent   :: !Markdown
    , pCreatedAt :: !UTCTime
    , pUpdatedAt :: !UTCTime
    , pTitle     :: !Text
    } deriving (Eq, Show)

type TableContent = [[Text]]

newtype Url = Url 
    { getUrl :: Text
    } deriving (Eq, Show)

newtype CodeName = CodeName 
    { getCodeName :: Text
    } deriving (Eq, Show)

newtype CodeSnippet = CodeSnippet
    { getCodeSnippet :: Text
    } deriving (Eq, Show)

-- | Markdown consist of list of 'Blocks'
newtype Markdown = Markdown 
    { getMarkdown :: [Block]
    } deriving (Eq, Show)

-- | Blocks are contents
data Block
    = BreakLine
    -- ^ Simply breaks a line
    | BlockQuote ScrapText
    -- ^ BlockQuote like markdown
    | BulletLine Int ScrapText
    -- ^ Bulletpoint styled line
    | BulletPoints [ScrapText]
    -- ^ Bullet points
    | CodeBlock CodeName CodeSnippet
    -- ^ Code blocks
    | Header Int Context
    -- ^ Header
    | Simple ScrapText
    -- ^ Simple text
    | Table TableContent -- No sure how to implement yet!!
    -- ^ Table
    | Thumbnail Url
    -- ^ Thumbnail
    deriving (Eq, Show)

-- | ScrapText are consisted by list of Scrap which are context associated with an style
newtype ScrapText = ScrapText {
    getScrapText :: [Scrap] 
    } deriving (Eq, Show)

-- | Scrap is an context which can have a style
data Scrap = Scrap
    { scrapStyle   :: !Style
    , scrapContent :: !Context
    } deriving (Eq, Show)

type Context = [ScrapContext]

-- | Style that can be applied to the 'Context'
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
    deriving (Eq, Show)

-- | Context
data ScrapContext =
      CodeNotation Text
    -- ^ CodeNotation
    | Link (Maybe Text) Url
    -- ^ Link, it can have href
    | PlainText Text
    -- ^ Just an simple text
    deriving (Eq, Show)

-- | StyleData
data StyleData = StyleData
    { bHeader        :: !Int
    -- ^ Size of an header,
    , bBold          :: !Bool
    -- ^ Bold style
    , bItalic        :: !Bool
    -- ^ Italic style
    , bStrikeThrough :: !Bool
    -- ^ Strike through
    } deriving (Eq, Show)
