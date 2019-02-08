{-| Test suites for testing parser on styled-text
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TestCommonMark.Styles
    ( styleSpec
    ) where

import           RIO

import           RIO.List              (headMaybe)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..))

import           Types                 (Block (..), InlineBlock (..),
                                        ScrapText (..), Segment (..),
                                        Style (..), isSimpleText)

import           TestCommonMark.Utils  (CommonMark (..), checkScrapbox,
                                        genPrintableText, getHeadSegment,
                                        getParagraph)

-- | Test suites for parsing styled text
styleSpec :: Spec
styleSpec = describe "Styles" $ do
    noStyleTextSpec
    boldTextSpec
    italicTextSpec

-- | Use Phantom type so we can generalize the test
newtype StyledText (a :: TestStyle) = StyledText
    { getStyledText :: Text
    } deriving Show

-- | Style type
--
-- Data constructors will be promoted using DataKinds
data TestStyle =
      BoldStyle
    | ItalicStyle
    | NoStyles

instance CommonMark (StyledText 'BoldStyle) where
    render (StyledText txt) = "**" <> txt <> "**"

instance CommonMark (StyledText 'ItalicStyle) where
    render (StyledText txt) = "*" <> txt <> "*"

instance CommonMark (StyledText 'NoStyles) where
    render (StyledText txt) = txt

instance Arbitrary (StyledText a) where
    arbitrary = StyledText <$> genPrintableText

-- | Generalized test case for checking whether the content of the text has same content
checkStyledTextContent :: (CommonMark (StyledText style)) => StyledText style -> Bool
checkStyledTextContent styledText =
    checkScrapbox styledText
        (\txt -> txt == getStyledText styledText)
        (\content -> do
            segment    <- getHeadSegment content
            (TEXT txt) <- getText segment
            return txt
        )
  where
    getText :: Segment -> Maybe Segment
    getText segment =
        if isSimpleText segment
        then Just segment
        else Nothing

getHeadInlineBlock :: [Block] -> Maybe Style
getHeadInlineBlock blocks = do
    blockContent                 <- headMaybe blocks
    (PARAGRAPH (ScrapText inlines)) <- getParagraph blockContent
    inline                          <- headMaybe inlines
    getStyle inline
  where
    getStyle :: InlineBlock -> Maybe Style
    getStyle (ITEM style _) = Just style
    getStyle _              = Nothing

--------------------------------------------------------------------------------
-- No style
--------------------------------------------------------------------------------

-- | Test spec for parsing non-styled text
noStyleTextSpec :: Spec
noStyleTextSpec =
    describe "Non-styled text" $ do
        prop "should parse non-styled text as NoStyle" $
           \(noStyleText :: StyledText 'NoStyles) ->
               checkScrapbox
                 noStyleText
                 (== NoStyle)
                 getHeadInlineBlock
        prop "should preserve its content" $
            \(noStyleText :: StyledText 'NoStyles) -> checkStyledTextContent noStyleText

--------------------------------------------------------------------------------
-- Bold text
--------------------------------------------------------------------------------

-- | Test spec for parsing Bold-styled text
boldTextSpec :: Spec
boldTextSpec = describe "Bold text" $ do
    prop "should parse bold text as Bold" $
        \(boldText :: StyledText 'BoldStyle) ->
            checkScrapbox
                boldText
                (== Bold)
                getHeadInlineBlock
    prop "should preserve its content" $
        \(boldText :: StyledText 'BoldStyle) -> checkStyledTextContent boldText

--------------------------------------------------------------------------------
-- Italic text
--------------------------------------------------------------------------------

-- | Test spec for parsing italic-styled text
italicTextSpec :: Spec
italicTextSpec = describe "Italic text" $ do
    prop "should parse italic text as Italic" $
        \(italicText :: StyledText 'ItalicStyle) ->
            checkScrapbox
                italicText
                (== Italic)
                getHeadInlineBlock
    prop "should preserve its content" $
        \(italicText :: StyledText 'ItalicStyle) -> checkStyledTextContent italicText
