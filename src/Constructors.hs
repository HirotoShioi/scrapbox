{-| Constructor that can be used to build scrapbox parse tree
-}

module Constructors
    ( markdown
    -- * Blocks
    , bulletList
    , blockQuote
    , bulletPoint
    , codeBlock
    , header
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

import           Types (Block (..), BulletSize (..), CodeName (..),
                        CodeSnippet (..), Content, Context (..),
                        HeaderSize (..), Markdown (..), ScrapText (..),
                        Segment (..), Style (..), StyleData (..),
                        TableContent (..), TableName (..), Url (..))

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

-- | Constructors for creating Markdown with given list of 'Block'
markdown :: [Block] -> Markdown
markdown = Markdown

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | Constructors for creating 'BlockQuote'
blockQuote :: [Context] -> Block
blockQuote = BlockQuote . ScrapText

-- | Constructors for creating 'BulletList' with given list of 'Block'
bulletList :: [Block] -> Block
bulletList = BulletList

-- | Constructors for creating 'CodeBlock' block
codeBlock :: Text -> Text -> Block
codeBlock codeName codeSnippet = CodeBlock (CodeName codeName) (CodeSnippet codeSnippet)

-- | Constructors for creating 'Paragraph' block
paragraph :: [Context] -> Block
paragraph = Paragraph . ScrapText

-- | Constructor for creating 'Paragraph' block, synonym of 'paragraph'
p :: [Context] -> Block
p = paragraph

-- | Constructors for creating 'Table' block
table :: Text -> [[Text]] -> Block
table title contents = Table (TableName title) (TableContent contents)

-- | Constructors for creating 'Thumbnail' with given Url
thumbnail :: Text -> Block
thumbnail url = Thumbnail (Url url)

-- | Constructors for creating 'Header' with given Header size and content
header :: Int -> Content -> Block
header size = Header (HeaderSize size)

-- | Constructors for creating 'BulletPoint' block with given size and content
bulletPoint :: Int -> [Context] -> Block
bulletPoint size ctxs = BulletPoint (BulletSize size) (ScrapText ctxs)

-- | 'LineBreak'
lineBreak :: Block
lineBreak = LineBreak

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

-- | Creates 'SimpleText' segment with given 'Text'
text :: Text -> Segment
text = SimpleText

-- | Creates 'CodeNotation' segment with given 'Text'
codeNotation :: Text -> Segment
codeNotation = CodeNotation

-- | Creates 'HashTag' segment with given 'Text'
hashtag :: Text -> Segment
hashtag = HashTag

-- | Creates 'Link' with given name and url
link :: Maybe Text -> Text -> Segment
link mName url = Link mName (Url url)

-- | Creates 'StyleData' with given params
styleData :: Int -> Bool -> Bool -> Bool -> StyleData
styleData = StyleData
