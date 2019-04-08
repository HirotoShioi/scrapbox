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

import           RIO.List (headMaybe)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Property)

import           Data.Scrapbox (Block (..), InlineBlock (..), ScrapText (..),
                                Segment (..), Style (..))
import           Data.Scrapbox.Internal (isText)
import           TestCommonMark.Utils (CommonMark (..), checkScrapbox,
                                       getHeadSegment, getParagraph)

import           Utils (genPrintableText)

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
    | StrikeThroughStyle

instance CommonMark (StyledText 'BoldStyle) where
    render (StyledText txt) = "**" <> txt <> "**"

instance CommonMark (StyledText 'ItalicStyle) where
    render (StyledText txt) = "*" <> txt <> "*"

instance CommonMark (StyledText 'StrikeThroughStyle) where
    render (StyledText txt) = "~~" <> txt <> "~~"

instance CommonMark (StyledText 'NoStyles) where
    render (StyledText txt) = txt

instance Arbitrary (StyledText a) where
    arbitrary = StyledText <$> genPrintableText

-- | Generalized test case for checking whether the content of the text has same content
checkStyledTextContent :: (CommonMark (StyledText style))
                       => StyledText style
                       -> Property
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
        if isText segment
        then Just segment
        else Nothing

getHeadInlineBlock :: [Block] -> Maybe [Style]
getHeadInlineBlock blocks = do
    blockContent                    <- headMaybe blocks
    (PARAGRAPH (ScrapText inlines)) <- getParagraph blockContent
    inline                          <- headMaybe inlines
    getStyle inline
  where
    getStyle :: InlineBlock -> Maybe [Style]
    getStyle (ITEM styles _) = Just styles
    getStyle _               = Nothing

--------------------------------------------------------------------------------
-- No style
--------------------------------------------------------------------------------

checkParse :: (CommonMark (StyledText a)) => Style -> StyledText a -> Property
checkParse style styledText =
    checkScrapbox
        styledText
        (\styles -> style `elem` styles)
        getHeadInlineBlock

-- | Test spec for parsing non-styled text
noStyleTextSpec :: Spec
noStyleTextSpec =
    describe "Non-styled text" $ do
        prop "should parse non-styled text as NoStyle" $
           \(noStyleText :: StyledText 'NoStyles) ->
               checkScrapbox
                 noStyleText
                 null
                 getHeadInlineBlock
        prop "should preserve its content" $
            \(noStyleText :: StyledText 'NoStyles) ->
                checkStyledTextContent noStyleText

-- | Test spec for parsing Bold-styled text
boldTextSpec :: Spec
boldTextSpec = describe "Bold text" $ do
    prop "should parse bold text as Bold" $
        \(boldText :: StyledText 'BoldStyle) ->
            checkScrapbox
                boldText
                (== [Bold])
                getHeadInlineBlock
    prop "should preserve its content" $
        \(boldText :: StyledText 'BoldStyle) -> checkStyledTextContent boldText

-- | Test spec for parsing italic-styled text
italicTextSpec :: Spec
italicTextSpec = describe "Italic text" $ do
    prop "should parse italic text as Italic" $
        \(italicText :: StyledText 'ItalicStyle) -> checkParse Italic italicText
    prop "should preserve its content" $
        \(italicText :: StyledText 'ItalicStyle) -> checkStyledTextContent italicText

-- | Test spec for parsing italic-styled text
strikeThroughTextSpec :: Spec
strikeThroughTextSpec = describe "Strikethrough text" $ do
    prop "should parse italic text as StrikeThrough" $
        \(strikeThroughText :: StyledText 'StrikeThroughStyle) -> 
            checkParse StrikeThrough strikeThroughText
    prop "should preserve its content" $
        \(strikeThroughText :: StyledText 'StrikeThroughStyle) -> 
            checkStyledTextContent strikeThroughText

-- | Test suites for parsing styled text
styleSpec :: Spec
styleSpec = describe "Styles" $ do
    noStyleTextSpec
    boldTextSpec
    italicTextSpec
    strikeThroughTextSpec
