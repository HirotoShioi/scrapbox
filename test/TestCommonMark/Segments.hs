{-| Test suites for testing parser on Segment
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Segments
    ( segmentSpec
    ) where

import           RIO

import           RIO.List              (headMaybe)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..))

import           Types                 (Block (..), Context (..),
                                        ScrapText (..), Segment (..), Url (..),
                                        isCodeNotation, isLink, isSimpleText)

import           TestCommonMark.Utils  (CommonMark (..), checkScrapbox,
                                        genPrintableText, genPrintableUrl,
                                        genRandomText, getHeadSegment,
                                        getParagraph)

-- | Test suites for 'Segment'
segmentSpec :: Spec
segmentSpec = describe "Segments" $ do
    linkSpec
    codeNotationSpec
    plainTextSpec

--------------------------------------------------------------------------------
-- Link
--------------------------------------------------------------------------------

-- | Link segment
data LinkSegment = LinkSegment
    { linkName :: !Text
    , linkUrl  :: !Text
    } deriving Show

instance CommonMark LinkSegment where
    render (LinkSegment name url) = "[" <> name <> "](" <> url <> ")"

instance Arbitrary LinkSegment where
    arbitrary = LinkSegment <$> genRandomText <*> genPrintableUrl

-- | Test spec for parsing 'LINK'
linkSpec :: Spec
linkSpec = describe "Links" $ do
    prop "should parse link as LINK" $
        \(linkSegment :: LinkSegment) ->
            checkScrapbox linkSegment isLink getHeadSegment

    prop "should preserve its content" $
        \(linkSegment :: LinkSegment) ->
            checkScrapbox linkSegment
                (\(mName, Url url) ->
                       url == linkUrl linkSegment
                    && maybe False (\name -> name == linkName linkSegment) mName

                )
                (\content -> do
                    segment          <- getHeadSegment content
                    (LINK mName url) <- getLink segment
                    return (mName, url)
                )
    prop "should not have any other segments except for code section" $
        \(linkSegment :: LinkSegment) -> testSegment linkSegment
  where
    getLink :: Segment -> Maybe Segment
    getLink linkSegment@(LINK _ _) = Just linkSegment
    getLink _                      = Nothing

--------------------------------------------------------------------------------
-- CodeNotation
--------------------------------------------------------------------------------

-- | 'CODE_NOTATION' segment
newtype CodeNotationSegment = CodeNotationSegment
    { getCodeNotationSegment :: Text
    } deriving Show

instance CommonMark CodeNotationSegment where
    render (CodeNotationSegment txt) = "`" <> txt <> "`"

instance Arbitrary CodeNotationSegment where
    arbitrary = CodeNotationSegment <$> genPrintableText

-- | Test spec for parsing 'CODE_NOTATION'
codeNotationSpec :: Spec
codeNotationSpec =
    describe "Code notation" $ do
        prop "should be able to parser code section as CODE_NOTATION" $
            \(codeNotation :: CodeNotationSegment) ->
                checkScrapbox codeNotation isCodeNotation getHeadSegment

        prop "should preserve its content" $
            \(codeNotation :: CodeNotationSegment) ->
                checkScrapbox codeNotation
                    (\codeText -> codeText == getCodeNotationSegment codeNotation)
                    (\content -> do
                        segment                 <- getHeadSegment content
                        (CODE_NOTATION codeText) <- getCodeNotationText segment
                        return codeText
                    )
        prop "should not have any other segments except for code section" $
            \(codeNotation :: CodeNotationSegment) -> testSegment codeNotation
  where
    getCodeNotationText :: Segment -> Maybe Segment
    getCodeNotationText codeNotation@(CODE_NOTATION _) = Just codeNotation
    getCodeNotationText _                              = Nothing

--------------------------------------------------------------------------------
-- Text segment
--------------------------------------------------------------------------------

-- | Text segment
newtype TextSegment = TextSegment
    { getTextSegment :: Text
    } deriving Show

instance CommonMark TextSegment where
    render (TextSegment txt) = txt

instance Arbitrary TextSegment where
    arbitrary = TextSegment <$> genPrintableText

-- | Test spec for parsing 'TEXT'
plainTextSpec :: Spec
plainTextSpec = describe "Plain text" $ do
    prop "should parse plain text as TEXT" $
        \(textSegment :: TextSegment) ->
            checkScrapbox textSegment isSimpleText getHeadSegment

    prop "should preserve its content" $
        \(textSegment :: TextSegment) ->
            checkScrapbox textSegment
            (\txt -> txt == getTextSegment textSegment)
            (\content -> do
                segment    <- getHeadSegment content
                (TEXT txt) <- getText segment
                return txt
            )

    prop "should not have any other segments except for plain text" $
        \(textSegment :: TextSegment) -> testSegment textSegment
  where
    getText :: Segment -> Maybe Segment
    getText textSegment@(TEXT _) = Just textSegment
    getText _                    = Nothing

-- | General test case to check whether the segment was parsed properly
testSegment :: (CommonMark section) => section -> Bool
testSegment someSegment =
    checkScrapbox someSegment
        (\(content', ctxs, segments) ->
               length content' == 1
            && length ctxs     == 1
            && length segments == 1
        )
        (\content -> do
            blockContent                 <- headMaybe content
            (PARAGRAPH (ScrapText ctxs)) <- getParagraph blockContent
            (Context _ segments)         <- headMaybe ctxs
            return (content, ctxs, segments)
        )
