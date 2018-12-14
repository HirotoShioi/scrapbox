module Constructors
    ( markdown
    -- * Blocks
    , textBlock
    , bulletList
    , blockQuote
    , bulletPoint
    , codeBlock
    , document
    , header
    , lineBreak
    , table
    , thumbnail
    -- * ScrapText
    , scrapText
    -- * Context
    , context
    -- * Constructors for creating 'Context' with a style
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
    , simpleText
    ) where

import           RIO   hiding (link)

import           Types

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

markdown :: [Block] -> Markdown
markdown = Markdown

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

textBlock :: Text -> Block
textBlock text = Document $ ScrapText [noStyle [simpleText text]]

blockQuote :: ScrapText -> Block
blockQuote = BlockQuote

bulletList :: [ScrapText] -> Block
bulletList = BulletList

codeBlock :: CodeName -> CodeSnippet -> Block
codeBlock = CodeBlock

document :: ScrapText -> Block
document = Document

table :: TableContent -> Block
table = Table

thumbnail :: Text -> Block
thumbnail url = Thumbnail (Url url)

header :: Int -> Content -> Block
header size contents = Header (HeaderSize size) contents

bulletPoint :: Int -> ScrapText -> Block
bulletPoint size stext = BulletPoint (BulletSize size) stext

lineBreak :: Block
lineBreak = LineBreak

--------------------------------------------------------------------------------
-- ScrapText
--------------------------------------------------------------------------------

scrapText :: [Context] -> ScrapText
scrapText = ScrapText

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

context :: Style -> Content -> Context
context = Context

noStyle :: [Segment] -> Context
noStyle segments = Context NoStyle segments

bold :: [Segment] -> Context
bold segments = Context Bold segments

italic :: [Segment] -> Context
italic segments = Context Italic segments

strikeThrough :: [Segment] -> Context
strikeThrough segments = Context StrikeThrough segments

customStyle :: StyleData -> [Segment] -> Context
customStyle sData segments = Context (CustomStyle sData) segments

--------------------------------------------------------------------------------
-- Segment
--------------------------------------------------------------------------------

simpleText :: Text -> Segment
simpleText = SimpleText

codeNotation :: Text -> Segment
codeNotation = CodeNotation

hashtag :: Text -> Segment
hashtag = HashTag

link :: Maybe Text -> Text -> Segment
link mName url = Link mName (Url url)

styleData :: Int -> Bool -> Bool -> Bool -> StyleData
styleData = StyleData
