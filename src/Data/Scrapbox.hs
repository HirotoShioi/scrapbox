{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
{-|
Module:      Scrapbox
Copyright:   (c) 2018-2019 Hiroto Shioi
License:     BSD3
Maintainer:  Hiroto Shioi <shioihigg@gmail.com>
Stability:   experimental
Portability: portable

Library which provides various tools for scrapbox. <https://scrapbox.io/product>
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
    , ParseOption
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
    , StyleData(..)
    ) where

import           RIO
import qualified RIO.Text                        as T

import           Data.List                       (nub)
import           Data.Scrapbox.Parser.Commonmark (parseCommonmark)
import           Data.Scrapbox.Parser.Scrapbox   (runScrapboxParser)
import           Data.Scrapbox.Render.Commonmark (renderToCommonmark)
import           Data.Scrapbox.Render.Scrapbox   (renderToScrapbox)
import           Data.Scrapbox.Types             (Block (..), CodeName (..),
                                                  CodeSnippet (..),
                                                  InlineBlock (..), Level (..),
                                                  ScrapText (..), Scrapbox (..),
                                                  Segment (..), Start (..),
                                                  Style (..), StyleData (..),
                                                  TableContent (..),
                                                  TableName (..), Url (..),
                                                  unverbose)
import           Network.URI                     (isURI)
import           Text.ParserCombinators.Parsec   (ParseError)

--------------------------------------------------------------------------------
-- Parse Option
--------------------------------------------------------------------------------

-- | Parser option which user can provide
data ParseOption
  = SectionHeading
  -- ^ Add 'LINEBREAK' before heading to make the content easier to read
  | FilterRelativePathLink
  -- ^ Remove relative paths such as @../foo/bar/baz.md@ when parsing link
  deriving (Eq)

-- | This parse option adds 'LINEBREAK' before each 'HEADING' to make it easier to see
optSectionHeading :: ParseOption
optSectionHeading = SectionHeading

-- | Remove relative path link such as @../foo/bar/baz.md@ when parsing 'LINK'
--
-- This option becomes useful when you use 'commonmarkToScrapbox'
optFilterRelativePathLink :: ParseOption
optFilterRelativePathLink = FilterRelativePathLink

-- | Apply changes to 'Scrapbox' based on the given @[ParseOption]@
applyOption :: [ParseOption] -> Scrapbox -> Scrapbox
applyOption options scrapbox = unverbose $ foldr apply scrapbox (nub options)
  where
    apply :: ParseOption -> Scrapbox -> Scrapbox
    apply SectionHeading (Scrapbox blocks)         = Scrapbox $ applyLinebreak blocks
    apply FilterRelativePathLink (Scrapbox blocks) = Scrapbox $ map applyFilterLink blocks

    -- Apply 'LINEBREAK' between 'HEADING' section
    -- >> [HEADING, b1, b2, b3, HEADING, b4, b5, b6, HEADING]
    -- >> [HEADING, b1, b2, b3, LINEBREAK, HEADING, b4, b5, b6, LINEBREAK, HEADING]
    applyLinebreak :: [Block] -> [Block]
    applyLinebreak []                             = []
    applyLinebreak [b]                            = [b]
    applyLinebreak (LINEBREAK : heading@(HEADING _ _) : rest) 
        = LINEBREAK : heading : applyLinebreak rest
    applyLinebreak (b:HEADING level content:rest)
        = b : LINEBREAK : applyLinebreak (HEADING level content : rest)
    applyLinebreak (b: rest)                      = b : applyLinebreak rest

    applyFilterLink :: Block -> Block
    applyFilterLink = \case
        PARAGRAPH (ScrapText inlines)   -> PARAGRAPH $ ScrapText $ map filterItem inlines
        BULLET_POINT start blocks       -> BULLET_POINT start $ map applyFilterLink blocks
        BLOCK_QUOTE (ScrapText inlines) -> BLOCK_QUOTE $ ScrapText $ map filterItem inlines
        HEADING level segments          -> HEADING level $ map filterRelativeLink segments
        other                           -> other

    filterItem :: InlineBlock -> InlineBlock
    filterItem = \case
      ITEM style segments -> ITEM style $ map filterRelativeLink segments
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
scrapboxToCommonmark :: [ParseOption] -> Text -> Either ParseError Text
scrapboxToCommonmark options scrapboxPage =
  renderToCommonmark <$> scrapboxToNode options scrapboxPage

-- | Parse given 'Scrapbox' formatted text into 'Scrapbox' AST
scrapboxToNode :: [ParseOption] -> Text -> Either ParseError Scrapbox
scrapboxToNode options scrapboxPage =
  applyOption options <$> runScrapboxParser (T.unpack scrapboxPage)

-- | Convert given commonmark text into 'Scrapbox' format
commonmarkToScrapbox :: [ParseOption] -> Text -> Text
commonmarkToScrapbox opts cmark = renderToScrapbox $ commonmarkToNode opts cmark

-- | Convert given commonmark into 'Scrapbox' AST
commonmarkToNode :: [ParseOption] -> Text -> Scrapbox
commonmarkToNode opts cmark = applyOption opts $ parseCommonmark cmark
