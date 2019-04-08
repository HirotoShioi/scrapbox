{-| Constructor that can be used to build scrapbox AST
-}

module Data.Scrapbox.Constructors
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
    , span
    , bold
    , italic
    , strikeThrough
    , mathExpr
    , codeNotation
    -- * Segment
    , hashtag
    , link
    , text
    ) where

import           RIO hiding (link, span)

import           Data.Scrapbox.Types (Block (..), CodeName (..),
                                      CodeSnippet (..), InlineBlock (..),
                                      Level (..), ScrapText (..), Scrapbox (..),
                                      Segment (..), Start (..), Style (..),
                                      TableContent (..), TableName (..),
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
codeBlock codeName codeSnippet = CODE_BLOCK
    (CodeName codeName)
    (CodeSnippet codeSnippet)

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

-- | Create inline with given 'Style' and 'Segment'
inline :: [Style] -> [Segment] -> InlineBlock
inline = ITEM

-- | Create 'ITEM' inline block
span :: [Style] -> [Segment] -> InlineBlock
span = inline

-- | Create 'InlineBlock' with bold style
bold :: Style
bold = Bold

-- | Creates 'InlineBlock' with italic style
italic :: Style
italic = Italic

-- | Creates 'InlineBlock' with strikethrough style
strikeThrough :: Style
strikeThrough = StrikeThrough

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
