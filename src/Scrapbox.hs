{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
{-|
Module:      Scrapbox
Copyright:   (c) 2018-2019 Hiroto Shioi
License:     BSD3
Maintainer:  Hiroto Shioi <shioihigg@gmail.com>
Stability:   experimental
Portability: portable

Scrapbox <https://scrapbox.io/product> parser.
-}
--------------------------------------------------------------------------------

module Scrapbox
    (
    -- * Converting commonmark to scrapbox
      commonmarkToScrapbox
    , commonmarkToNode
    -- * Converting scrapbox
    , scrapboxToCommonmark
    , scrapboxToNode
    -- ** Parse options
    , ParseOption
    , optDefault
    , optSectionHeading
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
import qualified RIO.Text                      as T

import           Scrapbox.Parser.Commonmark    (parseCommonmark)
import           Scrapbox.Parser.Scrapbox      (runScrapboxParser)
import           Scrapbox.Render.Commonmark    (renderToCommonmark)
import           Scrapbox.Render.Scrapbox      (renderToScrapbox)
import           Scrapbox.Types                (Block (..), CodeName (..),
                                                CodeSnippet (..),
                                                InlineBlock (..), Level (..),
                                                ScrapText (..), Scrapbox (..),
                                                Segment (..), Start (..),
                                                Style (..), StyleData (..),
                                                TableContent (..),
                                                TableName (..), Url (..),
                                                unverbose)
import           Text.ParserCombinators.Parsec (ParseError)

--------------------------------------------------------------------------------
-- Parse Option
--------------------------------------------------------------------------------

-- | Parser option which user can provide
data ParseOption
  = Default
  -- ^ Will convert 'CommonMark' to 'Scrapbox' format as is
  | SectionHeading
  -- ^ Will add 'LINEBREAK' before heading to make the content easier to read

-- | Default parse option
optDefault :: ParseOption
optDefault = Default

-- | This parse option adds 'LINEBREAK' before each 'HEADING' to make it easier to see
optSectionHeading :: ParseOption
optSectionHeading = SectionHeading

-- | Apply changes on 'Scrapbox' depending on the given 'ParseOption'
applyOption :: ParseOption -> Scrapbox -> Scrapbox
applyOption Default sb                       = unverbose sb
applyOption SectionHeading (Scrapbox blocks) = unverbose $ Scrapbox $ applyLinebreak blocks
  where
    -- Apply 'LINEBREAK' between 'HEADING' section
    --
    -- >> [HEADING, b1, b2, b3, HEADING, b4, b5, b6, HEADING]
    -- >> [HEADING, b1, b2, b3, LINEBREAK, HEADING, b4, b5, b6, LINEBREAK, HEADING]
    applyLinebreak :: [Block] -> [Block]
    applyLinebreak []                              = []
    applyLinebreak [b]                             = [b]
    applyLinebreak (b:HEADING level content:rest) =
        b : LINEBREAK : applyLinebreak (HEADING level content : rest)
    applyLinebreak (b: rest)                       = b : applyLinebreak rest

--------------------------------------------------------------------------------
-- Parse logic with options
--------------------------------------------------------------------------------

-- | Convert given scrapbox format text into commonmark format
scrapboxToCommonmark :: ParseOption -> Text -> Either ParseError Text
scrapboxToCommonmark option scrapboxPage =
  renderToCommonmark <$> scrapboxToNode option scrapboxPage

-- | Parse given scrapbox formatted text into 'Scrapbox' AST
scrapboxToNode :: ParseOption -> Text -> Either ParseError Scrapbox
scrapboxToNode option scrapboxPage =
  applyOption option <$> runScrapboxParser (T.unpack scrapboxPage)

-- | Convert given common mark text into 'Scrapbox' format
commonmarkToScrapbox :: ParseOption -> Text -> Text
commonmarkToScrapbox opt cmark = renderToScrapbox $ commonmarkToNode opt cmark

-- | Convert given common mark into 'Scrapbox' AST
commonmarkToNode :: ParseOption -> Text -> Scrapbox
commonmarkToNode opt cmark = applyOption opt $ parseCommonmark cmark
