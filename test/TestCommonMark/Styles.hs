{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Styles where

import           RIO

import           RIO.List              (headMaybe)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..))

import           Types                 (Block (..), Context (..),
                                        ScrapText (..), Segment (..),
                                        Style (..), isSimpleText)

import           TestCommonMark.Utils  (CommonMarkdown (..), checkMarkdown,
                                        genPrintableText, getHeadSegment,
                                        getParagraph)

--------------------------------------------------------------------------------
-- Something new
--------------------------------------------------------------------------------

-- | Use Phantom type to change typeclass instances
newtype StyledText a = StyledText {
    getStyledText :: Text
    } deriving Show

data BoldStyle
data ItalicStyle
data NoStyles

instance CommonMarkdown (StyledText BoldStyle) where
    render (StyledText txt) = "**" <> txt <> "**"

instance CommonMarkdown (StyledText ItalicStyle) where
    render (StyledText txt) = "*" <> txt <> "*"

instance CommonMarkdown (StyledText NoStyles) where
    render (StyledText txt) = txt

instance Arbitrary (StyledText a) where
    arbitrary = StyledText <$> genPrintableText

checkStyledTextContent :: (CommonMarkdown (StyledText style)) => StyledText style -> Bool
checkStyledTextContent styledText = do
    checkMarkdown styledText
        (\(SimpleText txt) -> txt == getStyledText styledText)
        (\content -> do
            segment <- getHeadSegment content
            if isSimpleText segment
                then Just segment
                else Nothing
        )

--------------------------------------------------------------------------------
-- No style
--------------------------------------------------------------------------------

noStyleTextSpec :: Spec
noStyleTextSpec = do
    describe "Non-styled text" $ do
        prop "should parse non-styled text as NoStyle" $
           \(noStyleText :: StyledText NoStyles) ->
               checkMarkdown noStyleText (\(Context style _) -> style == NoStyle) getHeadContext
        prop "should preserve its content" $
            \(noStyleText :: StyledText NoStyles) -> checkStyledTextContent noStyleText

--------------------------------------------------------------------------------
-- Bold text
--------------------------------------------------------------------------------

boldTextSpec :: Spec
boldTextSpec = describe "Bold text" $ do
    prop "should parse bold text as Bold" $
        \(boldText :: StyledText BoldStyle) ->
            checkMarkdown boldText (\(Context style _) -> style == Bold) getHeadContext
    prop "should preserve its content" $
        \(boldText :: StyledText BoldStyle) -> checkStyledTextContent boldText

--------------------------------------------------------------------------------
-- Italic text
--------------------------------------------------------------------------------

italicTextSpec :: Spec
italicTextSpec = describe "Italic text" $ do
    prop "should parse italic text as Italic" $
        \(italicText :: StyledText ItalicStyle) ->
            checkMarkdown italicText (\(Context style _) -> style == Italic) getHeadContext
    prop "should preserve its content" $
        \(italicText :: StyledText ItalicStyle) -> checkStyledTextContent italicText

getHeadContext :: [Block] -> Maybe Context
getHeadContext blocks = do
    blockContent                 <- headMaybe blocks
    (Paragraph (ScrapText ctxs)) <- getParagraph blockContent
    headMaybe ctxs
