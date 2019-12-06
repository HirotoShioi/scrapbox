{-# LANGUAGE LambdaCase #-}

-- | Module which exports Exception type
module Data.Scrapbox.Exception
  ( ScrapboxError (..),
  )
where

import RIO
import qualified RIO.Text as T
import qualified Text.ParserCombinators.Parsec as Parsec

data ScrapboxError
  = -- | Failed to parse given text as Scrapbox
    ParseError Parsec.ParseError
  | -- | Failed to decode backup file
    FailedToDecodeBackupJSON !String
  | -- | Failed to parse backup file
    FailedToParsePage !Text
  deriving (Show, Eq)

instance Exception ScrapboxError where
  displayException = \case
    FailedToDecodeBackupJSON s ->
      "Failed to decode backup json file with reason: "
        <> s
    FailedToParsePage s -> "Failed to parse page: " <> T.unpack s
    ParseError parseError -> "Failed to parse given text with error: " <> show parseError
