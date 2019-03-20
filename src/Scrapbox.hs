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
    -- ** Parse options
    , ParseOption
    , optDefault
    , optSectionHeading
    -- * Converting scrapbox
    , scrapboxToCommonmark
    , scrapboxToNode
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

import           Scrapbox.Parser.Commonmark    (ParseOption,
                                                commonmarkToScrapbox,
                                                commonmarkToNode,
                                                optDefault, optSectionHeading)
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
                                                TableName (..), Url (..))
import           Text.ParserCombinators.Parsec (ParseError)

-- Perhaps add option?

-- | Convert given scrapbox format text into commonmark format
scrapboxToCommonmark :: Text -> Either ParseError Text
scrapboxToCommonmark scrapbox = renderToCommonmark <$> scrapboxToNode scrapbox

-- | Parse given scrapbox formatted text into 'Scrapbox' AST
scrapboxToNode :: Text -> Either ParseError Scrapbox
scrapboxToNode = runScrapboxParser . T.unpack
