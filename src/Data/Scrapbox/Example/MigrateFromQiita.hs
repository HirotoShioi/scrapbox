{- This modules is an example of how one can use the scrapbox library to create an backup.json
of an Qiita blog posts of given user.
-}

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Scrapbox.Example.MigrateFromQiita
    ( ScrapboxBackup(..)
    , ScrapboxPage(..)
    , QiitaBlogPost(..)
    , Config(..)
    , defaultConfig
    , mkScrapboxBackupJSON
    , backupToMd
    ) where

import           Prelude

import           Control.Exception (Exception, SomeException, throwIO, try)
import           Control.Monad (forM)
import           Data.Aeson (FromJSON (..), ToJSON (..), decodeStrict, object,
                             withObject, (.:), (.:?), (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Scrapbox (ParseError, commonmarkToScrapbox,
                                optFilterRelativePathLink, optSectionHeading,
                                scrapboxToCommonmark)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime,
                                        utcTimeToPOSIXSeconds)
import           Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

data QiitaBlogPost = QiitaBlogPost
    { qbpTitle     :: !Text
    , qbpCreatedAt :: !UTCTime
    , qbpBody      :: !Text
    } deriving Show

instance FromJSON QiitaBlogPost where
    parseJSON = withObject "qiita blog post" $ \o -> do
        title     <- o .: "title"
        createdAt <- o .: "created_at"
        body      <- o .: "body"
        return $ QiitaBlogPost title createdAt body

data ScrapboxPage = ScrapboxPage
    { spTitle   :: !Text
    -- ^ Title of the page
    , spCreated :: !Integer
    -- ^ Date created
    , spUpdated :: !Integer
    -- ^ Dates updated
    , spLines   :: ![Text]
    -- ^ Content of the page
    } deriving Show

instance ToJSON ScrapboxPage where
    toJSON (ScrapboxPage title created updated ls) = object
        [ "title"   .= title
        , "created" .= created
        , "updated" .= updated
        , "lines"   .= ls
        ]

instance FromJSON ScrapboxPage where
    parseJSON = withObject "Scrapbox page" $ \o -> do
        title   <- o .: "title"
        created <- o .: "created"
        updated <- o .: "updated"
        ls      <- o .: "lines"
        return $ ScrapboxPage title created updated ls

data ScrapboxBackup = ScrapboxBackup
    { sbName        :: !(Maybe Text)
    -- ^ Name (Optional)
    , sbDisplayname :: !(Maybe Text)
    -- ^ Display name (Optional)
    , sbExported    :: !Integer
    -- ^ Date exported
    , sbPages       :: ![ScrapboxPage]
    -- ^ Pages
    } deriving Show

newtype UserName = UserName String

instance ToJSON ScrapboxBackup where
    toJSON (ScrapboxBackup name displayname exported pages) = object
        [ "name"        .= name
        , "displayName" .= displayname
        , "exported"    .= exported
        , "pages"       .= pages
        ]

instance FromJSON ScrapboxBackup where
    parseJSON = withObject "Scrapbox backup" $ \o -> do
        name        <- o .:? "name"
        displayname <- o .:? "displayName"
        exported    <- o .:  "exported"
        pages       <- o .:  "pages"
        pure $ ScrapboxBackup name displayname exported pages

--------------------------------------------------------------------------------
-- These constants will be used within mkScrapboxBackupJSON
-- to fetch/store given Qiita blog posts
--------------------------------------------------------------------------------

data Config = Config
    { storePath     :: !FilePath
    , qiitaUserName :: !UserName
    , backupName    :: !String
    , backUpTitle   :: !Text
    }

defaultConfig :: Config
defaultConfig = Config {
          storePath = "./test-docs/"
        , qiitaUserName = UserName "Enter_Qiita_Username_Here"
        , backupName = "backup.json"
        , backUpTitle = "Qiita backup"
    }

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

-- | Convert given 'QiitaBlogPost' to 'ScrapboxPage'
toScrapboxPage :: QiitaBlogPost -> ScrapboxPage
toScrapboxPage (QiitaBlogPost title createdAt body) =
  let date       = round $ utcTimeToPOSIXSeconds createdAt
      parsedPage = commonmarkToScrapbox
        [optSectionHeading, optFilterRelativePathLink]
        body
  in  ScrapboxPage title date date (title : T.lines parsedPage)

toScrapboxBackup :: Text -> POSIXTime -> [QiitaBlogPost] -> ScrapboxBackup
toScrapboxBackup title createdTime qiitaPosts =
  let scrapboxpages = map toScrapboxPage qiitaPosts
  in  ScrapboxBackup (Just title) (Just title) (round createdTime) scrapboxpages

--------------------------------------------------------------------------------
-- Fetching/Storing
--------------------------------------------------------------------------------

-- | Fetch blogposts from Qiita, convert them into 'ScrapboxBackup' and storing it
-- into @storePath@
mkScrapboxBackupJSON :: Config -> IO ()
mkScrapboxBackupJSON config = do
    qiitaPosts <- fetchPosts (qiitaUserName config)
    currTime   <- getPOSIXTime
    let backup = toScrapboxBackup (backUpTitle config) currTime qiitaPosts
    BL.writeFile (storePath config <> backupName config) (encodePretty backup)
  where
    fetchPosts :: UserName -> IO [QiitaBlogPost]
    fetchPosts username = either
        (\(e :: SomeException) -> throwIO $ FailedToFetchPosts (show e))
        return
        =<<
        try (getQiitaBlogPostHttp username)

    getQiitaBlogPostHttp :: UserName -> IO [QiitaBlogPost]
    getQiitaBlogPostHttp (UserName username) = do
        req <- parseRequest $ mconcat
            [ "https://qiita.com/api/v2/items?query=user%3A"
            , username
            ]
        getResponseBody <$> httpJSON req

-- | Convert given scrapbox backupfile into commonmark format
backupToMd :: FilePath -> IO ()
backupToMd path = do
    scrapboxBackup <- readBackup
    mds            <- convertoCommonMark scrapboxBackup
    mapM_ (\(title, md) -> T.writeFile (path <> T.unpack title <> ".md") md) mds
  where
    readBackup :: IO ScrapboxBackup
    readBackup = maybe
        (throwIO FailedToReadBackUp)
        return
        =<< (decodeStrict <$> BS.readFile path)

    convertoCommonMark :: ScrapboxBackup -> IO [(Text, Text)]
    convertoCommonMark scrapboxBackup = either
        (const $ throwIO FailedToCreateMdFile)
        return
        (toCommonmarks scrapboxBackup)

    -- Create an list of pairs with (Title, Body)
    toCommonmarks :: ScrapboxBackup -> Either ParseError [(Text, Text)]
    toCommonmarks (ScrapboxBackup _ _ _ pages) = forM pages $ \page -> do
        let body = T.unlines $ spLines page
        md <- scrapboxToCommonmark [] body
        return (spTitle page, md)

data MigrationException
    = FailedToReadBackUp
    | FailedToCreateMdFile
    | FailedToFetchPosts String

instance Show MigrationException where
    show = \case
        FailedToReadBackUp     -> "Failed to read backup file"
        FailedToCreateMdFile   -> "Failed to create md files"
        FailedToFetchPosts str -> "Faild to fetch qiita blog posts with reason: " <> str

instance Exception MigrationException
