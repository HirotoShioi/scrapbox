{-| Module which exports Exception type
-}

{-# LANGUAGE LambdaCase #-}

module Data.Scrapbox.Exception
  ( ScrapboxError (..),
  )
where

import RIO
import qualified RIO.Text as T
import qualified Text.ParserCombinators.Parsec as Parsec

data ScrapboxError
  = ParseError Parsec.ParseError
  -- ^ Failed to parse given text as Scrapbox
  | FailedToDecodeBackupJSON !String
  -- ^ Failed to decode backup file
  | FailedToParsePage !Text
  -- ^ Failed to parse backup file
  deriving (Show, Eq)

instance Exception ScrapboxError where
  displayException = \case
    FailedToDecodeBackupJSON s ->
      "Failed to decode backup json file with reason: "
        <> s
    FailedToParsePage s -> "Failed to parse page: " <> T.unpack s
    ParseError parseError -> "Failed to parse given text with error: " <> show parseError
