module Types where

import RIO
import RIO.Time

-- https://scrapbox.io/help/Syntax

-- TODO: Make naming more easy to understand/ makes sense

-- | Data structure of an scrapbox page in JSON format
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
    | Header Int Content
    -- ^ Header
    | Simple ScrapText
    -- ^ Simple text
    | Table TableContent -- No sure how to implement yet!!
    -- ^ Table
    | Thumbnail Url
    -- ^ Thumbnail
    deriving (Eq, Show)

-- | ScrapText are consisted by list of Scrap which are contexts associated with an style
newtype ScrapText = ScrapText {
    getScrapText :: [Context] 
    } deriving (Eq, Show)

-- | Scrap is an context which can have a style
data Context = Context
    { scrapStyle   :: !Style
    , scrapContent :: !Content
    } deriving (Eq, Show)

type Content = [Segment]

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
    -- ^ StrikeThrough style
    deriving (Eq, Show)

-- | Context
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
    { bHeaderSize    :: !Int
    -- ^ Size of an header,
    , bBold          :: !Bool
    -- ^ Bold style
    , bItalic        :: !Bool
    -- ^ Italic style
    , bStrikeThrough :: !Bool
    -- ^ Strike through
    } deriving (Eq, Show)
