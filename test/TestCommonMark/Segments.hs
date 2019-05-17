{-| Test suites for testing parser on Segment
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Segments
    ( segmentSpec
    ) where

import           RIO

import           RIO.List (headMaybe)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Property, (.&&.), (===))

import           Data.Scrapbox (Block (..), InlineBlock (..), ScrapText (..),
                                Segment (..), Url (..))
import           Data.Scrapbox.Internal (genPrintableUrl)
import           TestCommonMark.Utils (checkScrapbox, getHeadInlineBlock,
                                       getHeadSegment, getParagraph)
import           Utils (Syntax (..), genNoSymbolText)

-- | Test suites for 'Segment'
segmentSpec :: Spec
segmentSpec = describe "Segments" $ modifyMaxSuccess (const 10000) $ do
    linkSpec
    codeNotationSpec
    plainTextSpec

--------------------------------------------------------------------------------
-- Link
--------------------------------------------------------------------------------

-- | Link segment
data LinkSegment = LinkSegment !Text !Text
    deriving Show

instance Syntax LinkSegment where
    render (LinkSegment name url) = "[" <> name <> "](" <> url <> ")"

instance Arbitrary LinkSegment where
    arbitrary = LinkSegment <$> genNoSymbolText <*> genPrintableUrl

-- | Test spec for parsing 'LINK'
linkSpec :: Spec
linkSpec = describe "Links" $ do
    prop "should preserve its content" $
        \linkSegment@(LinkSegment name url) ->
            checkScrapbox linkSegment
                (=== LINK (Just name) (Url url))
                getHeadSegment
    prop "should not have any other segments except for code section" $
        \(linkSegment :: LinkSegment) -> testSegment linkSegment

--------------------------------------------------------------------------------
-- CodeNotation
--------------------------------------------------------------------------------

-- | 'CODE_NOTATION' segment
newtype CodeNotationSegment = CodeNotationSegment Text
    deriving Show

instance Syntax CodeNotationSegment where
    render (CodeNotationSegment txt) = "`" <> txt <> "`"

instance Arbitrary CodeNotationSegment where
    arbitrary = CodeNotationSegment <$> genNoSymbolText

-- | Test spec for parsing 'CODE_NOTATION'
codeNotationSpec :: Spec
codeNotationSpec =
    describe "Code notation" $
        prop "should preserve its content" $
            \codeNotation@(CodeNotationSegment notation) ->
                checkScrapbox codeNotation
                    (=== CODE_NOTATION notation)
                    getHeadInlineBlock

--------------------------------------------------------------------------------
-- Text segment
--------------------------------------------------------------------------------

-- | Text segment
newtype TextSegment = TextSegment Text
    deriving Show

instance Syntax TextSegment where
    render (TextSegment txt) = txt

instance Arbitrary TextSegment where
    arbitrary = TextSegment <$> genNoSymbolText

-- | Test spec for parsing 'TEXT'
plainTextSpec :: Spec
plainTextSpec = describe "Plain text" $ do
    prop "should preserve its content" $
        \textSegment@(TextSegment txt) ->
            checkScrapbox textSegment
            (=== TEXT txt)
            getHeadSegment

    prop "should not have any other segments except for plain text" $
        \(textSegment :: TextSegment) -> testSegment textSegment

-- | General test case to check whether the segment was parsed properly
testSegment :: (Syntax section) => section -> Property
testSegment someSegment =
    checkScrapbox someSegment
        (\(content', inlines, segments) ->
                 length content' == 1
            .&&. length inlines  == 1
            .&&. length segments == 1
        )
        (\content -> do
            blockContent                    <- headMaybe content
            (PARAGRAPH (ScrapText inlines)) <- getParagraph blockContent
            (SPAN _ segments)               <- headMaybe inlines
            return (content, inlines, segments)
        )
