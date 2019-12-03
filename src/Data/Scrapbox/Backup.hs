{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Backup
  ( ScrapboxBackup (..),
    ScrapboxPage (..),
    fromBackup,
  )
where

import Data.Aeson ((.:), (.:?), (.=), FromJSON (..), ToJSON (..), eitherDecodeStrict, object, withObject)
import RIO
import Data.Scrapbox.Parser.Scrapbox
import qualified RIO.Text as T
import Data.Scrapbox.Render.Commonmark

data ScrapboxPage
  = ScrapboxPage
      { -- | Title of the page
        spTitle :: !Text,
        -- | Date created
        spCreated :: !Integer,
        -- | Dates updated
        spUpdated :: !Integer,
        -- | Content of the page
        spLines :: ![Text]
      }
  deriving (Show)

instance ToJSON ScrapboxPage where
  toJSON (ScrapboxPage title created updated ls) =
    object
      [ "title" .= title,
        "created" .= created,
        "updated" .= updated,
        "lines" .= ls
      ]

instance FromJSON ScrapboxPage where
  parseJSON = withObject "Scrapbox page" $ \o -> do
    title <- o .: "title"
    created <- o .: "created"
    updated <- o .: "updated"
    ls <- o .: "lines"
    return $ ScrapboxPage title created updated ls

data ScrapboxBackup
  = ScrapboxBackup
      { -- | Name (Optional)
        sbName :: !(Maybe Text),
        -- | Display name (Optional)
        sbDisplayname :: !(Maybe Text),
        -- | Date exported
        sbExported :: !Integer,
        -- | Pages
        sbPages :: ![ScrapboxPage]
      }
  deriving (Show)

instance ToJSON ScrapboxBackup where
  toJSON (ScrapboxBackup name displayname exported pages) =
    object
      [ "name" .= name,
        "displayName" .= displayname,
        "exported" .= exported,
        "pages" .= pages
      ]

instance FromJSON ScrapboxBackup where
  parseJSON = withObject "Scrapbox backup" $ \o -> do
    name <- o .:? "name"
    displayname <- o .:? "displayName"
    exported <- o .: "exported"
    pages <- o .: "pages"
    pure $ ScrapboxBackup name displayname exported pages

-- | Parse given backup json file into list of commonmark pages
fromBackup :: ByteString -> Either BackupError [Text]
fromBackup jsonByteString =
  either
    (\s -> Left $ FailedToDecodeBackupJSON s)
    (\backup -> mapM intoMarkdown $ sbPages backup
    )
    (eitherDecodeStrict jsonByteString)
  where
    intoMarkdown :: ScrapboxPage -> Either BackupError Text
    intoMarkdown (ScrapboxPage title _created _updated content') = do
        let content = T.unlines content'
        either
            (\_parseError -> Left $ FailedToParsePage title)
            (\parsed -> Right $ renderToCommonmarkNoOption parsed)
            (runScrapboxParser $ T.unpack content)

        

data BackupError
  = FailedToDecodeBackupJSON String
  | FailedToParsePage Text

instance Show BackupError where
  show = \case
    FailedToDecodeBackupJSON s -> "Failed to decode backup json file with reason: "
        <> s
    FailedToParsePage s -> "Failed to parse page: " <> T.unpack s
