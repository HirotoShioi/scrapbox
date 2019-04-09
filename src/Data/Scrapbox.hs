{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
{-|
Module:      Scrapbox
Copyright:   (c) 2018-2019 Hiroto Shioi
License:     BSD3
Maintainer:  Hiroto Shioi <shioihigg@gmail.com>
Stability:   experimental
Portability: portable

Renderer and parser for scrapbox <https://scrapbox.io/product>
-}
--------------------------------------------------------------------------------

module Data.Scrapbox
    (
    -- * Converting commonmark
      commonmarkToScrapbox
    , commonmarkToNode
    -- * Converting scrapbox
    , scrapboxToCommonmark
    , scrapboxToNode
    -- ** Parse options
    , ScrapboxOption
    , optSectionHeading
    , optFilterRelativePathLink
    -- * Rendering Scrapbox AST
    , renderToScrapbox
    , renderToCommonmark
    -- * Data types
    , Scrapbox(..)
    , Block(..)
    , Url(..)
    , Level(..)
    , Start(..)
    , CodeName(..)
    , CodeSnippet(..)
    , TableName(..)
    , TableContent(..)
    -- ** Inline block
    , ScrapText(..)
    , InlineBlock(..)
    , Segment(..)
    , Style(..)
    , ParseError
    ) where

import           RIO
import qualified RIO.Text as T

import           Data.List (nub)
import           Data.Scrapbox.Parser.Commonmark (parseCommonmark)
import           Data.Scrapbox.Parser.Scrapbox (runScrapboxParser)
import           Data.Scrapbox.Render.Commonmark (renderToCommonmarkNoOption)
import           Data.Scrapbox.Render.Scrapbox (renderToScrapboxNoOption)
import           Data.Scrapbox.Types (Block (..), CodeName (..),
                                      CodeSnippet (..), InlineBlock (..),
                                      Level (..), ScrapText (..), Scrapbox (..),
                                      Segment (..), Start (..), Style (..),
                                      TableContent (..), TableName (..),
                                      Url (..), unverbose)
import           Network.URI (isURI)
import           Text.ParserCombinators.Parsec (ParseError)

--------------------------------------------------------------------------------
-- Parse Option
--------------------------------------------------------------------------------

-- | Option which user can provide when parsing/rendering
data ScrapboxOption
  = SectionHeading
  -- ^ Add 'LINEBREAK' before heading to make the content easier to read
  | FilterRelativeLink
  -- ^ Remove relative link such as @../foo/bar/baz.md@ when parsing link
  deriving (Eq)

-- | This parse option adds 'LINEBREAK' before each 'HEADING' to make it easier
-- to see
optSectionHeading :: ScrapboxOption
optSectionHeading = SectionHeading

-- | Remove relative path link such as @../foo/bar/baz.md@ when parsing 'LINK'
--
-- This option becomes useful when you use 'commonmarkToScrapbox'
optFilterRelativePathLink :: ScrapboxOption
optFilterRelativePathLink = FilterRelativeLink

-- | Apply changes to 'Scrapbox' based on the given @[ScrapboxOption]@
applyOption :: [ScrapboxOption] -> Scrapbox -> Scrapbox
applyOption options scrapbox = unverbose $ foldr apply scrapbox (nub options)
  where
    apply :: ScrapboxOption -> Scrapbox -> Scrapbox
    apply SectionHeading (Scrapbox blocks)     = Scrapbox $ applyLinebreak blocks
    apply FilterRelativeLink (Scrapbox blocks) = Scrapbox $ map applyFilterLink blocks

    -- Apply 'LINEBREAK' between 'HEADING' section
    -- >> [HEADING, b1, b2, b3, HEADING, b4, b5, b6, HEADING]
    -- >> [HEADING, b1, b2, b3, LINEBREAK, HEADING, b4, b5, b6, LINEBREAK, HEADING]
    applyLinebreak :: [Block] -> [Block]
    applyLinebreak []  = []
    applyLinebreak [b] = [b]
    applyLinebreak (LINEBREAK : heading@(HEADING _ _) : rest)
        = LINEBREAK : heading : applyLinebreak rest
    applyLinebreak (b:HEADING level content:rest)
        = b : LINEBREAK : applyLinebreak (HEADING level content : rest)
    applyLinebreak (b: rest)
        = b : applyLinebreak rest

    applyFilterLink :: Block -> Block
    applyFilterLink = \case
        PARAGRAPH (ScrapText inlines)   ->
            PARAGRAPH $ ScrapText $ map filterSpan inlines
        BULLET_POINT start blocks       ->
            BULLET_POINT start $ map applyFilterLink blocks
        BLOCK_QUOTE (ScrapText inlines) ->
            BLOCK_QUOTE $ ScrapText $ map filterSpan inlines
        HEADING level segments          ->
            HEADING level $ map filterRelativeLink segments
        other                           -> other

    filterSpan :: InlineBlock -> InlineBlock
    filterSpan = \case
      SPAN style segments -> SPAN style $ map filterRelativeLink segments
      other               -> other

    filterRelativeLink :: Segment -> Segment
    filterRelativeLink = \case
      LINK (Just name) (Url url) ->
        if isURI (T.unpack url)
          then LINK (Just name) (Url url)
          else LINK Nothing (Url name)
      other -> other

--------------------------------------------------------------------------------
-- Parse logic with options
--------------------------------------------------------------------------------

-- | Convert given 'Scrapbox' format text into commonmark format
scrapboxToCommonmark :: [ScrapboxOption] -> Text -> Either ParseError Text
scrapboxToCommonmark options scrapboxPage =
  renderToCommonmarkNoOption <$> scrapboxToNode options scrapboxPage

-- | Parse given 'Scrapbox' formatted text into 'Scrapbox' AST
scrapboxToNode :: [ScrapboxOption] -> Text -> Either ParseError Scrapbox
scrapboxToNode options scrapboxPage =
  applyOption options <$> runScrapboxParser (T.unpack scrapboxPage)

-- | Convert given commonmark text into 'Scrapbox' format
commonmarkToScrapbox :: [ScrapboxOption] -> Text -> Text
commonmarkToScrapbox opts cmark = renderToScrapboxNoOption
    $ commonmarkToNode opts cmark

-- | Convert given commonmark into 'Scrapbox' AST
commonmarkToNode :: [ScrapboxOption] -> Text -> Scrapbox
commonmarkToNode opts cmark = applyOption opts
    $ parseCommonmark (applyCorrection cmark)

-- | Render given 'Scrapbox' AST into commonmark with given @[ScrapboxOption]@
renderToCommonmark :: [ScrapboxOption] -> Scrapbox -> Text
renderToCommonmark opts scrapbox = renderToCommonmarkNoOption
    $ applyOption opts scrapbox

-- | Render given 'Scrapbox' AST into Scrapbox page with given @[ScrapboxOption]@
renderToScrapbox :: [ScrapboxOption] -> Scrapbox -> Text
renderToScrapbox opts scrapbox = renderToScrapboxNoOption
    $ applyOption opts scrapbox

-- | Apply correction to ensure that the @CMark@ parses the syntaxes correctly
--
-- 1. Have a space inserted on heading
--
-- >> "##Example" -> "## Example"
applyCorrection :: Text -> Text
applyCorrection = T.unlines . map apply . T.lines
  where
    apply :: Text -> Text
    apply line
      |    "#" `T.isPrefixOf` line
        && not (" " `T.isPrefixOf` T.dropWhile (== '#') line) =
        let (symbol, rest) = T.break (/= '#') line
        in symbol <> " " <> rest
      | otherwise = line
