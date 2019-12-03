{-| Datatypes used to express the parsed text into structured tree as well as some of the helper
-- functions.
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

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
    ) where

import           RIO hiding (span)

import           Data.List (groupBy, nub, sort, sortBy)
import           Data.Scrapbox.Utils (genMaybe, genNonSpaceText1,
                                      genPrintableText, genPrintableUrl,
                                      shortListOf1, shortListOf)
import           Test.QuickCheck (Arbitrary (..), choose, elements, frequency,
                                  genericShrink, listOf, oneof, resize, sized)

-- | Scrapbox page are consisted by list of 'Block's
newtype Scrapbox = Scrapbox [Block]
    deriving (Eq, Show, Generic, Read, Ord)

--------------------------------------------------------------------------------
-- Many of the Arbitrary instance are implemented in such a way that it can be
-- tested on roundtrip test. (i.e. Remove ambiguity so the parsing is trivial)
--------------------------------------------------------------------------------

instance Arbitrary Scrapbox where
    arbitrary = sized $ \size -> resize (adjustSize size) $ do
        (Scrapbox blocks) <- unverbose . Scrapbox <$> shortListOf1 arbitrary
        return $ Scrapbox $ removeAmbiguity blocks
        where
            adjustSize num | num < 30 = num
                           | otherwise = 30
    shrink (Scrapbox blocks) = map (unverbose . Scrapbox . removeAmbiguity) $ shrink blocks

-- | Replace ambiguous @[Block]@ with something else. Used for testing.
removeAmbiguity :: [Block] -> [Block]
removeAmbiguity = \case
    [] -> []

    (TABLE name (TableContent [[]]) : rest) ->
        TABLE name (TableContent []) : removeAmbiguity rest

    -- Need to take a look
    (BULLET_POINT (Start num1) (BULLET_POINT (Start num2) bs : cs) : rest) ->
        let b = if null cs
            then [ BULLET_POINT (Start $ num1 + num2) bs ]
            else [ BULLET_POINT (Start $ num1 + num2) bs
                 , BULLET_POINT (Start num1) cs
                 ]
        in removeAmbiguity (b <> rest)

    -- Add LINEBREAK after BULLETPOINT, CODE_BLOCK, and TABLE
    [BULLET_POINT s b] -> [BULLET_POINT s (removeAmbiguity b)]
    (BULLET_POINT start blocks : _x : xs) ->
          BULLET_POINT start (removeAmbiguity blocks)
        : LINEBREAK
        : removeAmbiguity xs
    (c@(CODE_BLOCK _ _) : _x : xs)  -> c : LINEBREAK : removeAmbiguity xs
    (t@(TABLE _ _): _x : xs)        -> t : LINEBREAK : removeAmbiguity xs
    -- Replace Link with THUMBNAIL
    (PARAGRAPH (ScrapText []) : xs) -> LINEBREAK : removeAmbiguity xs
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
    arbitrary = CodeSnippet <$> shortListOf1 genPrintableText
    shrink = genericShrink

-- | Heading level
newtype Level = Level Int
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary Level where
    arbitrary = Level <$> choose (2, 5)

-- | Name of the table
newtype TableName = TableName Text
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary TableName where
    arbitrary = TableName <$> genNonSpaceText1

-- | Content of the table
newtype TableContent = TableContent [[Text]]
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary TableContent where
    arbitrary = TableContent . align <$> shortListOf (shortListOf genPrintableText)
      where
        align :: [[Text]] -> [[Text]]
        align = sortBy (flip (\ a b -> length a `compare` length b))
    shrink (TableContent content) =
        map TableContent
            $ filter (not . any null)
            $ shrink content

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
    arbitrary = replaceAmbiguousBlock <$> oneof
            [ return LINEBREAK
            , BLOCK_QUOTE <$> arbitrary
            , BULLET_POINT <$> arbitrary <*> (removeAmbiguity <$> shortListOf1 arbitrary)
            , CODE_BLOCK <$> arbitrary <*> arbitrary
            , HEADING <$> arbitrary <*> (concatSegment . addSpace <$> shortListOf1 arbitrary)
            , PARAGRAPH <$> arbitrary
            , TABLE <$> arbitrary <*> arbitrary
            , THUMBNAIL <$> arbitrary
            ]
    shrink = \case
        BULLET_POINT s b ->
            map (replaceAmbiguousBlock . BULLET_POINT s) $ genericShrink b
        others -> map replaceAmbiguousBlock . genericShrink $ others

replaceAmbiguousBlock :: Block -> Block
replaceAmbiguousBlock = \case
    PARAGRAPH (ScrapText [])                           -> LINEBREAK
    PARAGRAPH (ScrapText [SPAN [] []])                 -> LINEBREAK
    PARAGRAPH (ScrapText [SPAN [Sized level] content]) -> HEADING level content
    PARAGRAPH (ScrapText [SPAN [] [LINK Nothing url]]) -> THUMBNAIL url
    BULLET_POINT _s []     -> LINEBREAK
    BULLET_POINT s content -> BULLET_POINT s (removeAmbiguity content)
    block -> block
--------------------------------------------------------------------------------
-- ScrapText
--------------------------------------------------------------------------------

-- | ScrapText are consisted by list of 'InlineBlock'
newtype ScrapText = ScrapText [InlineBlock]
    deriving (Eq, Show, Generic, Read, Ord)

instance Arbitrary ScrapText where
    arbitrary = ScrapText . formatInline . concatInline <$> shortListOf1 arbitrary
    shrink (ScrapText inlines) = map (ScrapText . formatInline . concatInline) $ genericShrink inlines

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
        let randSpan     = SPAN <$> arbitrary <*> shortListOf1 arbitrary
        let randCode     = CODE_NOTATION <$> genPrintableText
        let randMathExpr = MATH_EXPRESSION <$> genPrintableText
        frequency [ (7, randSpan)
                  , (1, randCode)
                  , (1, randMathExpr)
                  ]
    shrink = genericShrink

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
        let randomHashTag = HASHTAG <$> genNonSpaceText1
        let randomLink    = LINK
                <$> genMaybe genPrintableText
                <*> (Url <$> genPrintableUrl)
        let randomText = TEXT <$> genPrintableText
        frequency [ (1, randomHashTag)
                  , (2, randomLink)
                  , (7, randomText)
                  ]
    shrink = genericShrink

-- | Style that can be applied to the 'Segment'
data Style
    = Sized Level
    -- ^ Font size
    | Bold
    -- ^ Bold style
    | Italic
    -- ^ Italic style
    | StrikeThrough
    -- ^ StrikeThrough style
    | UserStyle !Text
    -- ^ User defined style
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
        foldl' (\acc segment -> acc <> [SPAN style [segment]]) mempty segments
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
        ScrapText $ sortStyle $ concatMap concatInline $ groupBy isSameStyle inlines

    isSameStyle :: InlineBlock -> InlineBlock -> Bool
    isSameStyle (SPAN style1 _) (SPAN style2 _) = style1 == style2
    isSameStyle _ _                             = False

    sortStyle [] = []
    sortStyle (SPAN style segment : xs) = SPAN (sort style) segment : sortStyle xs
    sortStyle (x : xs)                  = x : sortStyle xs

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
-- These functions are used to define typeclass instance of Arbitrary
--------------------------------------------------------------------------------

-- | Format 'InlineBlock'
formatInline :: [InlineBlock] -> [InlineBlock]
formatInline [] = []
formatInline [SPAN style segments]    = [SPAN style $ addSpace segments]
formatInline [inline]                 = [inline]
formatInline (SPAN style segments:xs) = SPAN style (addSpace segments) : formatInline xs
formatInline (x:xs)                   = x : formatInline xs

-- | Add space after hashtag
addSpace :: [Segment] -> [Segment]
addSpace []                               = []
addSpace [HASHTAG txt]                    = [HASHTAG txt]
addSpace (HASHTAG txt : TEXT text : rest) =
    HASHTAG txt : TEXT (" " <> text) : addSpace rest
addSpace (HASHTAG txt : rest)             =
    HASHTAG txt : TEXT " " : addSpace rest
addSpace (x:xs)                           = x : addSpace xs
