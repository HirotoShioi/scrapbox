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

import           TestCommonMark.Utils  (CommonMarkdown (..), checkMarkdown,
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

instance CommonMarkdown LinkSegment where
    render (LinkSegment name url) = "[" <> name <> "](" <> url <> ")"

instance Arbitrary LinkSegment where
    arbitrary = LinkSegment <$> genRandomText <*> genPrintableUrl

-- | Test spec for parsing 'Link'
linkSpec :: Spec
linkSpec = describe "Links" $ do
    prop "should parse link as Link" $
        \(linkSegment :: LinkSegment) ->
            checkMarkdown linkSegment isLink getHeadSegment

    prop "should preserve its content" $
        \(linkSegment :: LinkSegment) ->
            checkMarkdown linkSegment
                (\(LINK mName (Url url)) ->
                       url == linkUrl linkSegment
                    && maybe False (\name -> name == linkName linkSegment) mName

                )
                (\content -> do
                    segment <- getHeadSegment content
                    getLink segment
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

-- | Code notation segment
newtype CodeNotationSegment = CodeNotationSegment
    { getCodeNotationSegment :: Text
    } deriving Show

instance CommonMarkdown CodeNotationSegment where
    render (CodeNotationSegment txt) = "`" <> txt <> "`"

instance Arbitrary CodeNotationSegment where
    arbitrary = CodeNotationSegment <$> genPrintableText

-- | Test spec for parsing 'CodeNotation'
codeNotationSpec :: Spec
codeNotationSpec =
    describe "Code notation" $ do
        prop "should be able to parser code section as CodeNotation" $
            \(codeNotation :: CodeNotationSegment) ->
                checkMarkdown codeNotation isCodeNotation getHeadSegment

        prop "should preserve its content" $
            \(codeNotation :: CodeNotationSegment) ->
                checkMarkdown codeNotation
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
    getCodeNotationText _                             = Nothing

--------------------------------------------------------------------------------
-- SimpleText segment
--------------------------------------------------------------------------------

-- | Simple text segment
newtype SimpleTextSegment = SimpleTextSegment {
    getSimpleTextSegment :: Text
    } deriving Show

instance CommonMarkdown SimpleTextSegment where
    render (SimpleTextSegment txt) = txt

instance Arbitrary SimpleTextSegment where
    arbitrary = SimpleTextSegment <$> genPrintableText

-- | Test spec for parsing 'SimpleText'
plainTextSpec :: Spec
plainTextSpec = describe "Plain text" $ do
    prop "should parse plain text as SimpleText" $
        \(simpleTextSegment :: SimpleTextSegment) ->
            checkMarkdown simpleTextSegment isSimpleText getHeadSegment

    prop "should preserve its content" $
        \(simpleTextSegment :: SimpleTextSegment) ->
            checkMarkdown simpleTextSegment
            (\(TEXT txt) -> txt == getSimpleTextSegment simpleTextSegment)
            (\content -> do
                segment                 <- getHeadSegment content
                getText segment
            )

    prop "should not have any other segments except for plain text" $
        \(simpleTextSegment :: SimpleTextSegment) -> testSegment simpleTextSegment
  where
    getText :: Segment -> Maybe Segment
    getText textSegment@(TEXT _) = Just textSegment
    getText _                    = Nothing

-- | General test case to check whether the segment was parsed properly
testSegment :: (CommonMarkdown section) => section -> Bool
testSegment someSegment =
    checkMarkdown someSegment
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
