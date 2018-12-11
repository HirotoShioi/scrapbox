module Types where

import RIO
import RIO.Time

-- https://scrapbox.io/help/Syntax

data Page = Page
    { pContent   :: Content
    , pCreatedAt :: UTCTime
    , pUpdatedAt :: UTCTime
    , pTitle     :: Text
    } deriving (Eq, Show)

newtype Content = Content 
    { getContent :: [Markdown]
    } deriving (Eq, Show)

data Markdown
    = BreakLine
    | BlockQuote ScrapText
    | BulletPoints [ScrapText]
    | CodeBlock CodeName Code
    | BulletLine ScrapText
    | Simple ScrapText
    | Table TableContent -- No sure how to implement yet!!
    | Thumbnail Url
    deriving (Eq, Show)

data ScrapText = 
      CodeNotation Text ScrapText
    | Styled Style ScrapText ScrapText
    | SimpleText Text ScrapText
    | Link (Maybe Text) Url ScrapText
    | EndLine
    deriving (Eq, Show)

type TableContent = [Text]
type Code = Text

newtype Url = Url 
    { getUrl :: Text
    } deriving (Eq, Show)

newtype CodeName = CodeName 
    { getCodeName :: Text
    } deriving (Eq, Show)

data Style = Style
    { bHeader        :: !Int
    , bBold          :: !Bool
    , bItalic        :: !Bool
    , bStrikeThrough :: !Bool
    } deriving (Eq, Show)
