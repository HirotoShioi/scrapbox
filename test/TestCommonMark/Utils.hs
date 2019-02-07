{-| Utility functions used to test commonmark parser
-}

{-# LANGUAGE OverloadedStrings #-}

module TestCommonMark.Utils
    ( CommonMark(..)
    , checkScrapbox
    , genPrintableText
    , getHeadSegment
    , getHeadContext
    , getParagraph
    , genRandomText
    , genPrintableUrl
    ) where

import           RIO

import           RIO.List        (headMaybe)
import qualified RIO.Text        as T
import           Test.QuickCheck (Gen, elements, listOf1)

import           CommonMark.Lib  (commonmarkToScrapboxNode, optDefault)
import           Types           (Block (..), Context (..), ScrapText (..),
                                  Scrapbox (..), Segment)

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

-- | Typeclass in which is used to render given datatype into markdown format.
class CommonMark a where
    render :: a -> Text

-- | Generate arbitrary Text
-- this is needed as some characters like
-- '`' and `>` will be parsed as blockquote, code notation, etc.
genPrintableText :: Gen Text
genPrintableText = T.unwords <$> listOf1 genRandomText

-- | Generate random text
genRandomText :: Gen Text
genRandomText = fmap fromString <$> listOf1 
    $ elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])

-- | Generate random url
genPrintableUrl :: Gen Text
genPrintableUrl = do
    end        <- elements [".org", ".edu", ".com", ".co.jp", ".io", ".tv"]
    randomSite <- genRandomText
    return $ "http://www." <> randomSite <> end

-- | General function used to test if given 'CommonMark' can be properly parsed
-- and extract the expected element
checkScrapbox :: (CommonMark a)
              => a
              -> (parsedContent -> Bool)
              -> ([Block] -> Maybe parsedContent)
              -> Bool
checkScrapbox markdown pre extractionFunc = do
    let (Scrapbox content) = parseMarkdown markdown
    maybe False pre (extractionFunc content)
  where
    -- | Parse given datatype into 'Scrapbox'
    parseMarkdown :: CommonMark a => a -> Scrapbox
    parseMarkdown = commonmarkToScrapboxNode optDefault . render

-- | Return 'PARAGRAPH' if given 'Block' is 'PARAGRAPH'
getParagraph :: Block -> Maybe Block
getParagraph paragraph@(PARAGRAPH _) = Just paragraph
getParagraph _                       = Nothing

-- | Extract heed segment of a given list of blocks
getHeadSegment :: [Block] -> Maybe Segment
getHeadSegment blocks = do
    blockContent               <- headMaybe blocks
    PARAGRAPH (ScrapText ctxs) <- getParagraph blockContent
    CONTEXT _ segments         <- headMaybe ctxs
    headMaybe segments

-- | Extract heed segment of a given list of blocks
getHeadContext :: [Block] -> Maybe Context
getHeadContext blocks = do
    blockContent               <- headMaybe blocks
    PARAGRAPH (ScrapText ctxs) <- getParagraph blockContent
    headMaybe ctxs
