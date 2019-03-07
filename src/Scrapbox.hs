-- |
-- Module:      Scrapbox
-- Copyright:   (c) 2019 Hiroto Shioi
-- License:     BSD3
-- Maintainer:  Hiroto Shioi <shioihigg@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Scrapbox <https://scrapbox.io/product> parser.

module Scrapbox
    ( -- * Converting commonmark to scrapbox
      commonmarkToScrapbox
    , commonmarkToScrapboxNode
    -- ** Parse options
    , ParseOption
    , optDefault
    , optSectionHeading
    -- * Parsing Scrapbox
    , parseScrapbox
    -- * Rendering Scrapbox
    , renderPretty
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

import           Scrapbox.CommonMark.Lib  (ParseOption, commonmarkToScrapbox,
                                           commonmarkToScrapboxNode, optDefault,
                                           optSectionHeading)
import           Scrapbox.Parser.Scrapbox (parseScrapbox)
import           Scrapbox.Render          (renderPretty)
import           Scrapbox.Types           (Block (..), CodeName (..),
                                           CodeSnippet (..), InlineBlock (..),
                                           Level (..), ScrapText (..),
                                           Scrapbox (..), Segment (..),
                                           Style (..), StyleData (..),
                                           TableContent (..), TableName (..),
                                           Url (..), Start(..))
