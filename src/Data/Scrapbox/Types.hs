{-| Datatypes used to represent the scrapbox AST as well as some of the helper
-- functions.
-}

{-# LANGUAGE DeriveGeneric     #-}
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
    , StyleData(..)
    , TableContent(..)
    -- * Helper functions
    , concatInline
    , concatSegment
    , concatScrapText
    , verbose
    , unverbose
    , emptyStyle
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
    , isNoStyle
    ) where

import           RIO

import           Data.List       (groupBy)
import           Data.Scrapbox.Utils  (genMaybe, genPrintableText, genPrintableUrl,
                                  genText)
import           Test.QuickCheck (Arbitrary (..), choose, frequency,
                                  genericShrink, getSize, listOf1, scale,
                                  vectorOf)

-- | Scrapbox page consist of list of 'Block'
newtype Scrapbox = Scrapbox [Block]
    deriving (Eq, Show, Generic, Read, Ord)

--------------------------------------------------------------------------------
-- Many of the Arbitrary instance are implemented in such a way that it can be
-- tested on roundtrip test. (i.e. Remove ambiguity so the parsing is trivial)
--------------------------------------------------------------------------------

instance Arbitrary Scrapbox where
    arbitrary = do
        newSize <- choose (1,10)
        scale (\size -> if size < 10 then size else newSize) $ do
            (Scrapbox blocks) <- unverbose . Scrapbox <$> listOf1 arbitrary
            return $ Scrapbox $ removeAmbiguity blocks
        where
          removeAmbiguity :: [Block] -> [Block]
          removeAmbiguity = \case
            [] -> []

            -- Add LINEBREAK after BULLETPOINT, CODE_BLOCK, and TABLE
            (BULLET_POINT start blocks : xs) ->
                BULLET_POINT start (removeAmbiguity blocks) : LINEBREAK : removeAmbiguity xs
            (c@(CODE_BLOCK _ _):xs) -> c : LINEBREAK : removeAmbiguity xs
            (t@(TABLE _ _): xs) -> t : LINEBREAK : removeAmbiguity xs

            -- Replace Link with THUMBNAIL
            (PARAGRAPH (ScrapText [ITEM NoStyle [LINK Nothing url]]): xs) ->
                THUMBNAIL url : removeAmbiguity xs

            -- Apply formatInlines to blocks
            (PARAGRAPH (ScrapText inlines): xs) ->
                PARAGRAPH (ScrapText $ formatInline inlines) : removeAmbiguity xs
            (BLOCK_QUOTE (ScrapText inlines): xs) ->
                BLOCK_QUOTE (ScrapText $ formatInline inlines) : removeAmbiguity xs
            (x:xs) -> x : removeAmbiguity xs
    shrink = genericShrink

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
    arbitrary = CodeName <$> genText

-- | Code snippet
newtype CodeSnippet = CodeSnippet [Text]
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary CodeSnippet where
    arbitrary = CodeSnippet <$> listOf1 genPrintableText
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
    arbitrary = TableName <$> genText

-- | Content of the table
newtype TableContent = TableContent [[Text]]
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary TableContent where
    arbitrary = do
        newSize <- choose (1,5)
        scale (\size -> if size < 5 && size > 0 then size else newSize) $ do
            num <- getSize
            TableContent <$> listOf1 (vectorOf num genText)
    shrink = genericShrink

-- | Url for Link/Thumbnail
newtype Url = Url Text
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary Url where
    arbitrary = Url <$> genPrintableUrl

-- | Scrapbox page is consisted by list of Blocks
data Block
    = LINEBREAK
    -- ^ Linebreak
    | BLOCK_QUOTE !ScrapText
    -- ^ BlockQuote
    | BULLET_POINT !Start ![Block]
    -- ^ Bulletpoint styled line
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
    arbitrary = do
        newSize <- choose (1,10)
        scale (\size -> if size < 10 then size else newSize) $ frequency
            [ (2, return LINEBREAK)
            , (1, BLOCK_QUOTE <$> arbitrary)
            , (1, BULLET_POINT <$> arbitrary <*> listOf1 bulletPointFreq)
            , (1, CODE_BLOCK <$> arbitrary <*> arbitrary)
            , (2, HEADING <$> arbitrary <*> (concatSegment . addSpace <$> listOf1 arbitrary))
            , (7, PARAGRAPH <$> arbitrary)
            , (1, TABLE <$> arbitrary <*> arbitrary)
            , (2, THUMBNAIL <$> arbitrary)
            ]
        where
          bulletPointFreq = frequency
            [ (1, BLOCK_QUOTE <$> arbitrary)
            , (1, CODE_BLOCK <$> arbitrary <*> arbitrary)
            , (2, HEADING <$> arbitrary <*> (concatSegment . addSpace <$> listOf1 arbitrary))
            , (7, PARAGRAPH <$> arbitrary)
            , (1, TABLE <$> arbitrary <*> arbitrary)
            , (2, THUMBNAIL <$> arbitrary)
            ]
    shrink = genericShrink

--------------------------------------------------------------------------------
-- ScrapText
--------------------------------------------------------------------------------

-- | ScrapText are consisted by list of 'InlineBlock'
newtype ScrapText = ScrapText [InlineBlock]
    deriving (Eq, Show, Generic, Read, Ord)

-- FIND ME
instance Arbitrary ScrapText where
    arbitrary = do
        newSize <- choose (0, sizeNum)
        scale (\size -> if size < sizeNum then size else newSize) $
            ScrapText . formatInline . concatInline <$> listOf1 arbitrary
      where
        sizeNum :: Int
        sizeNum = 10
    shrink = genericShrink

-- | InlineBlock
data InlineBlock
    = ITEM !Style ![Segment]
    -- ^ ITEM are blocks which can have styles
    | CODE_NOTATION !Text
    -- ^ Code notation
    | MATH_EXPRESSION !Text
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary InlineBlock where
    arbitrary = do
        let randItem     = ITEM <$> arbitrary <*> listOf1 arbitrary
        let randCode     = CODE_NOTATION <$> genPrintableText
        let randMathExpr = MATH_EXPRESSION <$> genPrintableText
        frequency [ (7, randItem)
                  , (1, randCode)
                  , (1, randMathExpr)
                  ]
    shrink = genericShrink

instance Arbitrary Text where
    arbitrary = fromString <$> arbitrary

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
        let randomHashTag = HASHTAG <$> genText
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
    = CustomStyle StyleData
    -- ^ You can use this to combine all three as of the styles as well as Header
    | Bold
    -- ^ Bold style
    | Italic
    -- ^ Italic style
    | NoStyle
    -- ^ No styles
    | StrikeThrough
    -- ^ StrikeThrough style
    | UserStyle Text
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary Style where
    arbitrary = do
        let randCustomStyle = CustomStyle <$> arbitrary
        frequency
            [ (1, randCustomStyle)
            , (1, return $ UserStyle "!?%")
            , (1, return Bold)
            , (1, return Italic)
            , (1, return StrikeThrough)
            , (8, return NoStyle)
            ]

-- | StyleData
data StyleData = StyleData
    { sHeaderSize    :: !Int
    -- ^ Size of an header,
    , sBold          :: !Bool
    -- ^ Bold style
    , sItalic        :: !Bool
    -- ^ Italic style
    , sStrikeThrough :: !Bool
    -- ^ Strike through
    } deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary StyleData where
    arbitrary = StyleData
        <$> choose (2,4)
        <*> return False
        <*> return True
        <*> return True

--------------------------------------------------------------------------------
-- Verbose/Unverbose
--------------------------------------------------------------------------------

-- | Empty style data
emptyStyle :: StyleData
emptyStyle = StyleData 0 False False False

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
    mkVerboseInlineBlock (ITEM style segments) =
        foldr (\segment acc -> [ITEM style [segment]] <> acc) mempty segments
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
    isSameStyle (ITEM style1 _) (ITEM style2 _) = style1 == style2
    isSameStyle _ _                             = False

-- | Concatinate 'ITEM' with same style
concatInline :: [InlineBlock] -> [InlineBlock]
concatInline []                       = []
concatInline [ITEM style inline]      = [ITEM style (concatSegment inline)]
concatInline [inline]                 = [inline]
concatInline (item1@(ITEM style1 inline1): item2@(ITEM style2 inline2) :rest)
    | style1 == style2 = concatInline (ITEM style1 (concatSegment $ inline1 <> inline2) : rest)
    | otherwise        = concatItem item1 : concatItem item2 : rest
concatInline (item@(ITEM _ _) : rest) = concatItem item : concatInline rest
concatInline (a : rest)               = a : concatInline rest

-- | Concatenate the content of 'ITEM'
concatItem :: InlineBlock -> InlineBlock
concatItem (ITEM style inline) = ITEM style (concatSegment inline)
concatItem others              = others

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

-- | Checks whether given 'Style' is 'NoStyle'
isNoStyle :: Style -> Bool
isNoStyle NoStyle = True
isNoStyle _       = False

--------------------------------------------------------------------------------
-- These functions are used to define typeclass instance of Arbitrary
--------------------------------------------------------------------------------

-- | Format 'InlineBlock'
formatInline :: [InlineBlock] -> [InlineBlock]
formatInline [] = []
formatInline [ITEM style segments]    = [ITEM style $ addSpace segments]
formatInline [inline]                 = [inline]
formatInline (ITEM style segments:xs) = ITEM style (addSpace segments) : formatInline xs
formatInline (x:xs)                   = x : formatInline xs

-- Add space after hashtag
addSpace :: [Segment] -> [Segment]
addSpace []                               = []
addSpace (HASHTAG txt : TEXT text : rest) = HASHTAG txt : TEXT (" " <> text) : addSpace rest
addSpace (HASHTAG txt : rest)             = HASHTAG txt : TEXT " " : addSpace rest
addSpace (x:xs)                           = x : addSpace xs
