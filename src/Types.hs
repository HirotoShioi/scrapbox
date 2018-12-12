module Types where

import RIO
import RIO.Time

-- https://scrapbox.io/help/Syntax

data Page = Page
    { pContent   :: !Markdown
    , pCreatedAt :: !UTCTime
    , pUpdatedAt :: !UTCTime
    , pTitle     :: !Text
    } deriving (Eq, Show)

newtype Markdown = Markdown 
    { getContent :: [Block]
    } deriving (Eq, Show)

data Block
    = BreakLine
    | BlockQuote ScrapText
    | BulletLine ScrapText
    | BulletPoints [ScrapText]
    | CodeBlock CodeName Code
    | Header Int ScrapText
    | Simple ScrapText
    | Table TableContent -- No sure how to implement yet!!
    | Thumbnail Url
    deriving (Eq, Show)

newtype ScrapText = ScrapText {
    getScrapText :: [Scrap] 
    } deriving (Eq, Show)

data Scrap = Scrap
    { sStyle :: Style
    , sContent :: Context
    } deriving (Eq, Show)

data Style = 
      Custom StyleData
    | Bold
    | Italic
    | None
    | StrikeThrough
    deriving (Eq, Show)

data Context =
      CodeNotation Text
    | Link (Maybe Text) Url
    | PlainText Text
    deriving (Eq, Show)

type TableContent = [[Text]]
type Code = Text

newtype Url = Url 
    { getUrl :: Text
    } deriving (Eq, Show)

newtype CodeName = CodeName 
    { getCodeName :: Text
    } deriving (Eq, Show)

data StyleData = StyleData
    { bHeader        :: !Int
    , bBold          :: !Bool
    , bItalic        :: !Bool
    , bStrikeThrough :: !Bool
    } deriving (Eq, Show)
