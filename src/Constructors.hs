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
    -- * InlineBlock
    , inline
    , noStyle
    , bold
    , italic
    , strikeThrough
    , mathExpr
    , codeNotation
    , customStyleBlock
    -- * Segment
    , hashtag
    , link
    , text
    ) where

import           RIO   hiding (link)

import           Types (Block (..), CodeName (..), CodeSnippet (..),
                        InlineBlock (..), Level (..), ScrapText (..),
                        Scrapbox (..), Segment (..), Start (..), Style (..),
                        StyleData (..), TableContent (..), TableName (..),
                        Url (..))

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

-- | Constructors for creating 'Scrapbox' with given list of 'Block'
scrapbox :: [Block] -> Scrapbox
scrapbox = Scrapbox

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | Constructors for creating 'BLOCK_QUOTE'
blockQuote :: [InlineBlock] -> Block
blockQuote = BLOCK_QUOTE . ScrapText

-- | Constructors for creating 'CODE_BLOCK' block
codeBlock :: Text -> [Text] -> Block
codeBlock codeName codeSnippet = CODE_BLOCK (CodeName codeName) (CodeSnippet codeSnippet)

-- | Constructors for creating 'PARAGRAPH' block
paragraph :: [InlineBlock] -> Block
paragraph = PARAGRAPH . ScrapText

-- | Constructor for creating 'PARAGRAPH' block, synonym of 'paragraph'
p :: [InlineBlock] -> Block
p = paragraph

-- | Constructors for creating 'TABLE' block
table :: Text -> [[Text]] -> Block
table title contents = TABLE (TableName title) (TableContent contents)

-- | Constructors for creating 'THUMBNAIL' with given 'Url'
thumbnail :: Text -> Block
thumbnail url = THUMBNAIL (Url url)

-- | Constructors for creating 'HEADING' with given level and content
heading :: Int -> [Segment] -> Block
heading level = HEADING (Level level)

-- | Constructors for creating 'BULLET_POINT' block with given start and content
bulletPoint :: Int -> [Block] -> Block
bulletPoint start = BULLET_POINT (Start start)

-- | 'LINEBREAK'
lineBreak :: Block
lineBreak = LINEBREAK

--------------------------------------------------------------------------------
-- InlineBlock
--------------------------------------------------------------------------------

-- | Create inline wigh given 'Style' and 'Segment'
inline :: Style -> [Segment] -> InlineBlock
inline = ITEM

-- | Creates 'InlineBlock' with no style
noStyle :: [Segment] -> InlineBlock
noStyle = ITEM NoStyle

-- | Create 'InlineBlock' with bold style
bold :: [Segment] -> InlineBlock
bold = ITEM Bold

-- | Creates 'InlineBlock' with italic style
italic :: [Segment] -> InlineBlock
italic = ITEM Italic

-- | Creates 'InlineBlock' with strikethrough style
strikeThrough :: [Segment] -> InlineBlock
strikeThrough = ITEM StrikeThrough

-- | Creates 'InlineBlock' wigh given 'StyleData' and 'Segment'
customStyleBlock :: StyleData -> [Segment] -> InlineBlock
customStyleBlock sData = ITEM (CustomStyle sData)

-- | Creates 'CODE_NOTATION' inline block with given 'Text'
codeNotation :: Text -> InlineBlock
codeNotation = CODE_NOTATION

-- | Creates 'CODE_NOTATION' inline block with given 'Text'
mathExpr :: Text -> InlineBlock
mathExpr = MATH_EXPRESSION

--------------------------------------------------------------------------------
-- Segment
--------------------------------------------------------------------------------

-- | Creates 'TEXT' segment with given 'Text'
text :: Text -> Segment
text = TEXT

-- | Creates 'HASHTAG' segment with given 'Text'
hashtag :: Text -> Segment
hashtag = HASHTAG

-- | Creates 'LINK' with given name and url
link :: Maybe Text -> Text -> Segment
link mName url = LINK mName (Url url)
