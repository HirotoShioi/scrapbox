{-# LANGUAGE OverloadedStrings #-}

module TestCommonMark.Utils where

import           RIO

import           RIO.List        (headMaybe)
import qualified RIO.Text        as T
import           Test.QuickCheck (Gen, elements, listOf1)

import           CommonMark.Lib  (commonmarkToMarkdown, optDefault)
import           Types           (Block (..), Context (..), Markdown (..),
                                  ScrapText (..), Segment)

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

-- | Typeclass in which is used to render given datatype into common markdown format.
class CommonMarkdown a where
    render :: a -> Text

-- | Generate arbitrary Text
-- this is needed as some characters like
-- '`' and `>` will be parsed as blockquote, code notation, etc.
genPrintableText :: Gen Text
genPrintableText = T.unwords <$> listOf1 genRandomText

-- | Generate random text
genRandomText :: Gen Text
genRandomText = (fmap fromString) <$> listOf1 $ elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])

-- | Generate random url
genPrintableUrl :: Gen Text
genPrintableUrl = do
    end        <- elements [".org", ".edu", ".com", ".co.jp"]
    randomSite <- genRandomText
    return $ "http://www." <> randomSite <> end

-- | Parse given datatype into Markdown
parseMarkdown :: CommonMarkdown a => a -> Markdown
parseMarkdown = commonmarkToMarkdown optDefault . render

-- | General function used to test if given 'CommonMarkdown' can be properly parsed
-- and extract the expected element
checkMarkdown :: (CommonMarkdown a)
              => a
              -> (parsedContent -> Bool)
              -> ([Block] -> Maybe parsedContent)
              -> Bool
checkMarkdown markdown pre extractionFunc = do
    let (Markdown content) = parseMarkdown markdown
    maybe False pre (extractionFunc content)

getParagraph :: Block -> Maybe Block
getParagraph paragraph@(Paragraph _) = Just paragraph
getParagraph _                       = Nothing

-- | Extract heed segment of a given list of blocks
getHeadSegment :: [Block] -> Maybe Segment
getHeadSegment blocks = do
    blockContent                 <- headMaybe blocks
    (Paragraph (ScrapText ctxs)) <- getParagraph blockContent
    (Context _ segments)         <- headMaybe ctxs
    segment                      <- headMaybe segments
    return segment
