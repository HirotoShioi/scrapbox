{-| Datatypes used to represent the scrapbox AST as well as some of the helper
-- functions.
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- This is to avoid warnings regarding defining typeclass instance of 'Text'
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Scrapbox.Types
    ( -- * Datatypes
      Scrapbox (..)
    , Start(..)
    , Block(..)
    , CodeName(..)
    , CodeSnippet(..)
    , Level(..)
    , TableName(..)
    , Url(..)
    , Segment(..)
    , InlineBlock(..)
    , ScrapText(..)
    , Style(..)
    , TableContent(..)
    -- * Helper functions
    , concatInline
    , concatSegment
    , concatScrapText
    , verbose
    , unverbose
    -- * Predicates
    , isBlockQuote
    , isBulletPoint
    , isCodeBlock
    , isCodeNotation
    , isMathExpr
    , isHeader
    , isLink
    , isParagraph
    , isThumbnail
    , isTable
    , isText
    , isHashTag
    , isBold
    , isItalic
    , isStrikeThrough
    ) where

import           RIO hiding (span)

import           Data.List (groupBy, nub, sort)
import           Data.Scrapbox.Utils (genMaybe, genNonSpaceText,
                                      genPrintableText, genPrintableUrl,
                                      shortListOf)
import           Test.QuickCheck (Arbitrary (..), choose, elements,
                                  frequency, genericShrink, listOf, oneof,
                                  resize, sized)

-- | Scrapbox page are consisted by list of 'Block's
newtype Scrapbox = Scrapbox [Block]
    deriving (Eq, Show, Generic, Read, Ord)

--------------------------------------------------------------------------------
-- Many of the Arbitrary instance are implemented in such a way that it can be
-- tested on roundtrip test. (i.e. Remove ambiguity so the parsing is trivial)
--------------------------------------------------------------------------------

instance Arbitrary Scrapbox where
    arbitrary = sized $ \size -> resize (adjustSize size) $ do
        (Scrapbox blocks) <- unverbose . Scrapbox <$> shortListOf arbitrary
        return $ Scrapbox $ removeAmbiguity blocks
        where
            adjustSize num | num < 50 = num
                           | otherwise = 50
    shrink (Scrapbox blocks) = map (Scrapbox . removeAmbiguity) $ shrink blocks

removeAmbiguity :: [Block] -> [Block]
removeAmbiguity = \case
    [] -> []
    -- Add LINEBREAK after BULLETPOINT, CODE_BLOCK, and TABLE
    (BULLET_POINT start blocks : xs) ->
          BULLET_POINT start (removeAmbiguity blocks)
        : LINEBREAK
        : removeAmbiguity xs
    (c@(CODE_BLOCK _ _):xs) -> c : LINEBREAK : removeAmbiguity xs
    (t@(TABLE _ _): xs)     -> t : LINEBREAK : removeAmbiguity xs
    -- Replace Link with THUMBNAIL
    (PARAGRAPH (ScrapText []) : xs)           -> LINEBREAK : removeAmbiguity xs

    (b@(BLOCK_QUOTE (ScrapText [SPAN _ inlines])) : xs) ->
        if null inlines
            then LINEBREAK : xs
            else b : removeAmbiguity xs
    (x:xs) -> x : removeAmbiguity xs

--------------------------------------------------------------------------------
-- Elements that are used in Block
--------------------------------------------------------------------------------

-- | Starting point of 'BULLET_POINT'
newtype Start = Start Int
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary Start where
    arbitrary = Start <$> choose (1,3)

-- | Name of the code block
newtype CodeName = CodeName Text
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary CodeName where
    arbitrary = CodeName <$> genPrintableText

-- | Code snippet
newtype CodeSnippet = CodeSnippet [Text]
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary CodeSnippet where
    arbitrary = CodeSnippet <$> shortListOf genPrintableText
    shrink    = genericShrink

-- | Heading level
newtype Level = Level Int
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary Level where
    arbitrary = Level <$> choose (2, 5)

-- | Name of the table
newtype TableName = TableName Text
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary TableName where
    arbitrary = TableName <$> genPrintableText

-- | Content of the table
newtype TableContent = TableContent [[Text]]
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary TableContent where
    arbitrary = TableContent <$> shortListOf (shortListOf genPrintableText)
    shrink = genericShrink

-- | Url for Link/Thumbnail
newtype Url = Url Text
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary Url where
    arbitrary = Url <$> genPrintableUrl

-- | 'Block' can be the following
data Block
    = LINEBREAK
    -- ^ Linebreak
    | BLOCK_QUOTE !ScrapText
    -- ^ BlockQuote
    | BULLET_POINT !Start ![Block]
    -- ^ Bulletpoint
    | CODE_BLOCK !CodeName !CodeSnippet
    -- ^ Code blocks
    | HEADING !Level ![Segment]
    -- ^ Header
    | PARAGRAPH !ScrapText
    -- ^ Paragraph
    | TABLE !TableName !TableContent
    -- ^ Table
    | THUMBNAIL !Url -- ambigious, maybe remove this
    -- ^ Thumbnail
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary Block where
    arbitrary = replaceAmbiguousBlock <$> frequency
            [ (2, return LINEBREAK)
            , (1, BLOCK_QUOTE <$> arbitrary)
            , (1, BULLET_POINT <$> arbitrary <*> shortListOf bulletPointFreq)
            , (1, CODE_BLOCK <$> arbitrary <*> arbitrary)
            , (2, HEADING <$> arbitrary <*> (concatSegment . addSpace <$> shortListOf arbitrary))
            , (3, PARAGRAPH <$> arbitrary)
            , (1, TABLE <$> arbitrary <*> arbitrary)
            , (2, THUMBNAIL <$> arbitrary)
            ]
        where
          bulletPointFreq = frequency
            [ (1, BLOCK_QUOTE <$> arbitrary)
            , (1, CODE_BLOCK <$> arbitrary <*> arbitrary)
            , (2, HEADING <$> arbitrary <*>
                (concatSegment . addSpace <$> shortListOf arbitrary)
              )
            , (3, PARAGRAPH <$> arbitrary)
            , (1, TABLE <$> arbitrary <*> arbitrary)
            , (2, THUMBNAIL <$> arbitrary)
            ]
    shrink = map replaceAmbiguousBlock . genericShrink

replaceAmbiguousBlock :: Block -> Block
replaceAmbiguousBlock = \case
    PARAGRAPH (ScrapText []) -> LINEBREAK
    PARAGRAPH (ScrapText [SPAN [] []]) -> LINEBREAK
    PARAGRAPH (ScrapText [SPAN [Sized level] content]) -> HEADING level content
    PARAGRAPH (ScrapText [SPAN [] [LINK Nothing url]]) -> THUMBNAIL url
    BULLET_POINT _s [] -> LINEBREAK
    BULLET_POINT s content -> BULLET_POINT s (map replaceAmbiguousBlock content)
    block -> block
--------------------------------------------------------------------------------
-- ScrapText
--------------------------------------------------------------------------------

-- | ScrapText are consisted by list of 'InlineBlock'
newtype ScrapText = ScrapText [InlineBlock]
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary ScrapText where
    arbitrary = ScrapText . formatInline . concatInline <$> shortListOf arbitrary
    shrink (ScrapText inlines) = map (ScrapText . formatInline . concatInline) $ shrink inlines

-- | InlineBlock
data InlineBlock
    = SPAN ![Style] ![Segment]
    -- ^ SPAN are blocks which can have styles
    | CODE_NOTATION !Text
    -- ^ Code notation
    | MATH_EXPRESSION !Text
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary InlineBlock where
    arbitrary = do
        let randSpan     = SPAN <$> arbitrary <*> shortListOf arbitrary
        let randCode     = CODE_NOTATION <$> genPrintableText
        let randMathExpr = MATH_EXPRESSION <$> genPrintableText
        frequency [ (7, randSpan)
                  , (1, randCode)
                  , (1, randMathExpr)
                  ]
    shrink = genericShrink

instance Arbitrary Text where
    arbitrary = genPrintableText

-- | Segment
data Segment
    = HASHTAG !Text
    -- ^ Hashtag
    | LINK !(Maybe Text) !Url
    -- ^ Link, it can have named as href
    | TEXT !Text
    -- ^ Just an simple text
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary Segment where
    arbitrary = do
        let randomHashTag = HASHTAG <$> genNonSpaceText
        let randomLink    = LINK
                <$> genMaybe genPrintableText
                <*> (Url <$> genPrintableUrl)
        let randomText    = TEXT <$> genPrintableText
        frequency [ (1, randomHashTag)
                  , (2, randomLink)
                  , (7, randomText)
                  ]
    shrink = genericShrink

-- | Style that can be applied to the 'Segment'
data Style
    = Sized Level
    -- ^ You can use this to combine all three as of the styles as well as Header
    | Bold
    -- ^ Bold style
    | Italic
    -- ^ Italic style
    | StrikeThrough
    -- ^ StrikeThrough style
    | UserStyle !Text
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary Style where
    arbitrary = do
        someLvl <- arbitrary
        elements
            [ UserStyle "!?%"
            , Bold
            , Italic
            , StrikeThrough
            , Sized someLvl
            ]

instance {-# OVERLAPS #-} Arbitrary [Style] where
    arbitrary = do
        somelvl <- arbitrary
        let someStyle = fmap (sort . nub) <$> listOf $ elements [Italic, StrikeThrough]
        oneof
            [ someStyle
            , return [Bold]
            , return [UserStyle "!?%"]
            , (\l -> sort $ l <> [Sized somelvl]) <$> someStyle
            ]
--------------------------------------------------------------------------------
-- Verbose/Unverbose
--------------------------------------------------------------------------------

-- | Convert given 'Scrapbox' into verbose structure
verbose :: Scrapbox -> Scrapbox
verbose (Scrapbox blocks) = Scrapbox $ map convertToVerbose blocks
  where
    convertToVerbose :: Block -> Block
    convertToVerbose = \case
        BLOCK_QUOTE stext      -> BLOCK_QUOTE $ verboseScrapText stext
        BULLET_POINT num block -> BULLET_POINT num $ map convertToVerbose block
        PARAGRAPH stext        -> PARAGRAPH $ verboseScrapText stext
        other                  -> other

    verboseScrapText :: ScrapText -> ScrapText
    verboseScrapText (ScrapText inlines) =
        ScrapText $ concatMap mkVerboseInlineBlock inlines

    mkVerboseInlineBlock :: InlineBlock -> [InlineBlock]
    mkVerboseInlineBlock (SPAN style segments) =
        foldr (\segment acc -> [SPAN style [segment]] <> acc) mempty segments
    mkVerboseInlineBlock others                = [others]

-- | Convert given 'Scrapbox' into unverbose structure
unverbose :: Scrapbox -> Scrapbox
unverbose (Scrapbox blocks) = Scrapbox $ map unVerboseBlock blocks
  where
    unVerboseBlock :: Block -> Block
    unVerboseBlock = \case
        BLOCK_QUOTE stext      -> BLOCK_QUOTE $ unVerboseScrapText stext
        BULLET_POINT num block -> BULLET_POINT num $ map unVerboseBlock block
        HEADING level segments -> HEADING level $ concatSegment segments
        PARAGRAPH stext        -> PARAGRAPH $ unVerboseScrapText stext
        other                  -> other

    unVerboseScrapText :: ScrapText -> ScrapText
    unVerboseScrapText (ScrapText inlines) =
        ScrapText $ concatMap concatInline $ groupBy isSameStyle inlines

    isSameStyle :: InlineBlock -> InlineBlock -> Bool
    isSameStyle (SPAN style1 _) (SPAN style2 _) = style1 == style2
    isSameStyle _ _                             = False

-- | Concatinate 'SPAN' with same style
concatInline :: [InlineBlock] -> [InlineBlock]
concatInline []                       = []
concatInline [SPAN style inline]      = [SPAN style (concatSegment inline)]
concatInline [inline]                 = [inline]
concatInline (span1@(SPAN style1 inline1): span2@(SPAN style2 inline2) :rest)
    | style1 == style2 = concatInline (SPAN style1 (concatSegment $ inline1 <> inline2) : rest)
    | otherwise        = concatSpan span1 : concatInline (concatSpan span2 : rest)
concatInline (span@(SPAN _ _) : rest) = concatSpan span : concatInline rest
concatInline (a : rest)               = a : concatInline rest

-- | Concatenate the content of 'SPAN'
concatSpan :: InlineBlock -> InlineBlock
concatSpan (SPAN style inline) = SPAN style (concatSegment inline)
concatSpan others              = others

-- | Concatenate 'ScrapText'
-- This could be Semigroup, but definitely not Monoid (there's no mempty)
concatScrapText :: ScrapText -> ScrapText -> ScrapText
concatScrapText (ScrapText inline1) (ScrapText inline2) =
    ScrapText . concatInline $ inline1 <> inline2

-- | Concatenate 'Segment'
concatSegment :: [Segment] -> [Segment]
concatSegment [] = []
concatSegment (TEXT txt1 : TEXT txt2 : rest) =
    concatSegment $ (TEXT $ txt1 <> txt2) : rest
concatSegment (a : rest) = a : concatSegment rest

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

-- | Checks if given 'Block' is an 'HEADING'
isHeader :: Block -> Bool
isHeader (HEADING _ _) = True
isHeader _             = False

-- | Checks whether given 'Block' is 'BLOCK_QUOTE'
isBlockQuote :: Block -> Bool
isBlockQuote (BLOCK_QUOTE _) = True
isBlockQuote _               = False

-- | Checks whether given 'Block' is 'BULLET_POINT'
isBulletPoint :: Block -> Bool
isBulletPoint (BULLET_POINT _ _) = True
isBulletPoint _                  = False

-- | Checks whether given 'Block' is 'CODE_BLOCK'
isCodeBlock :: Block -> Bool
isCodeBlock (CODE_BLOCK _ _) = True
isCodeBlock _                = False

-- | Checks whether given 'Block' is 'PARAGRAPH'
isParagraph :: Block -> Bool
isParagraph (PARAGRAPH _) = True
isParagraph _             = False

-- | Checks whether given 'Block' is 'THUMBNAIL'
isThumbnail :: Block -> Bool
isThumbnail (THUMBNAIL _) = True
isThumbnail _             = False

-- | Checks whether given 'Block' is 'TABLE'
isTable :: Block -> Bool
isTable (TABLE _ _) = True
isTable _           = False

-- | Checks whether given 'InlineBlock' is 'CODE_NOTATION'
isCodeNotation :: InlineBlock -> Bool
isCodeNotation (CODE_NOTATION _) = True
isCodeNotation _                 = False

-- | Checks whether given 'Inline' is 'MATH_EXPRESSION'
isMathExpr :: InlineBlock -> Bool
isMathExpr (MATH_EXPRESSION _) = True
isMathExpr _                   = False

-- | Checks whether given 'Segment' is 'TEXT'
isText :: Segment -> Bool
isText (TEXT _) = True
isText _        = False

-- | Checks whether given 'Segment' is 'LINK'
isLink :: Segment -> Bool
isLink (LINK _ _) = True
isLink _          = False

-- | Checks whether given 'Segment' is 'HASHTAG'
isHashTag :: Segment -> Bool
isHashTag (HASHTAG _) = True
isHashTag _           = False

-- | Checks whether given 'Style' is 'Bold'
isBold :: Style -> Bool
isBold Bold = True
isBold _    = False

-- | Checks whether given 'Style' is 'Italic'
isItalic :: Style -> Bool
isItalic Italic = True
isItalic _      = False

-- | Checks whether given 'Style' is 'StrikeThrough'
isStrikeThrough :: Style -> Bool
isStrikeThrough StrikeThrough = True
isStrikeThrough _             = False

--------------------------------------------------------------------------------
-- These functions are used to define typeclass instance of Arbitrary
--------------------------------------------------------------------------------

-- | Format 'InlineBlock'
formatInline :: [InlineBlock] -> [InlineBlock]
formatInline [] = []
formatInline [SPAN style segments]    = [SPAN style $ addSpace segments]
formatInline [inline]                 = [inline]
formatInline (SPAN style segments:xs) = SPAN style (addSpace segments) : formatInline xs
formatInline (x:xs)                   = x : formatInline xs

-- Add space after hashtag
addSpace :: [Segment] -> [Segment]
addSpace []                               = []
addSpace (HASHTAG txt : TEXT text : rest) =
    HASHTAG txt : TEXT (" " <> text) : addSpace rest
addSpace (HASHTAG txt : rest)             =
    HASHTAG txt : TEXT " " : addSpace rest
addSpace (x:xs)                           = x : addSpace xs
