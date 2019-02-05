{-| Constructor that can be used to build scrapbox AST
-}

module Constructors
    ( scrapbox
    -- * Blocks
    , blockQuote
    , bulletPoint
    , codeBlock
    , heading
    , lineBreak
    , paragraph
    , p
    , table
    , thumbnail
    -- * Context
    , context
    , noStyle
    , bold
    , italic
    , strikeThrough
    , customStyle
    -- * For creating custom style
    , styleData
    -- * Segment
    , codeNotation
    , hashtag
    , link
    , text
    ) where

import           RIO   hiding (link)

import           Types (Block (..), Start (..), CodeName (..),
                        CodeSnippet (..), Content, Context (..),
                        Level (..), Scrapbox (..), ScrapText (..),
                        Segment (..), Style (..), StyleData (..),
                        TableContent (..), TableName (..), Url (..))

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

-- | Constructors for creating Markdown with given list of 'Block'
scrapbox :: [Block] -> Scrapbox
scrapbox = Scrapbox

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | Constructors for creating 'BLOCK_QUOTE'
blockQuote :: [Context] -> Block
blockQuote = BLOCK_QUOTE . ScrapText

-- | Constructors for creating 'CODE_BLOCK' block
codeBlock :: Text -> Text -> Block
codeBlock codeName codeSnippet = CODE_BLOCK (CodeName codeName) (CodeSnippet codeSnippet)

-- | Constructors for creating 'PARAGRAPH' block
paragraph :: [Context] -> Block
paragraph = PARAGRAPH . ScrapText

-- | Constructor for creating 'PARAGRAPH' block, synonym of 'paragraph'
p :: [Context] -> Block
p = paragraph

-- | Constructors for creating 'TABLE' block
table :: Text -> [[Text]] -> Block
table title contents = TABLE (TableName title) (TableContent contents)

-- | Constructors for creating 'THUMBNAIL' with given 'Url'
thumbnail :: Text -> Block
thumbnail url = THUMBNAIL (Url url)

-- | Constructors for creating 'HEADING' with given level and content
heading :: Int -> Content -> Block
heading level = HEADING (Level level)

-- | Constructors for creating 'BULLET_POINT' block with given start and content
bulletPoint :: Int -> [Block] -> Block
bulletPoint start = BULLET_POINT (Start start)

-- | 'LINEBREAK'
lineBreak :: Block
lineBreak = LINEBREAK

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

-- | Create context wigh given 'Style' and 'Content'
context :: Style -> Content -> Context
context = Context

-- | Creates 'Context' with no style
noStyle :: [Segment] -> Context
noStyle = Context NoStyle

-- | Create 'Context' with bold style
bold :: [Segment] -> Context
bold = Context Bold

-- | Creates 'Context' with italic style
italic :: [Segment] -> Context
italic = Context Italic

-- | Creates 'Context' with strikethrough style
strikeThrough :: [Segment] -> Context
strikeThrough = Context StrikeThrough

-- | Creates 'Context' wigh given 'StyleData' and 'Segment'
customStyle :: StyleData -> [Segment] -> Context
customStyle sData = Context (CustomStyle sData)

--------------------------------------------------------------------------------
-- Segment
--------------------------------------------------------------------------------

-- | Creates 'TEXT' segment with given 'Text'
text :: Text -> Segment
text = TEXT

-- | Creates 'CODE_NOTATION' segment with given 'Text'
codeNotation :: Text -> Segment
codeNotation = CODE_NOTATION

-- | Creates 'HASHTAG' segment with given 'Text'
hashtag :: Text -> Segment
hashtag = HASHTAG

-- | Creates 'LINK' with given name and url
link :: Maybe Text -> Text -> Segment
link mName url = LINK mName (Url url)

-- | Creates 'StyleData' with given params
styleData :: Int -> Bool -> Bool -> Bool -> StyleData
styleData = StyleData
