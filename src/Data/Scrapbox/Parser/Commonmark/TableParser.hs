{-| This module exports parser used to parse common mark table
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Parser.Commonmark.TableParser
    ( parseTable
    , CommonMarkTable
    , Column
    ) where

import           RIO

import           Data.Attoparsec.Text  (Parser)
import qualified Data.Attoparsec.Text  as P
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Prelude               (String)

import           Data.Scrapbox.Constructors (table)
import           Data.Scrapbox.Types        (Block)

-- | Representation of CommonMark table
newtype CommonMarkTable = CommonMarkTable [Column]
    deriving Show

-- | Each column
newtype Column = Column
    { getColumn :: [Text]
    } deriving Show

-- Should fix these!

columnParser :: Parser Column
columnParser = do
    rest <- P.takeText
    go mempty rest
  where
    -- Seems like it's Either monad but I need an function which converts
    -- Either to Parser (natural transformation?)
    go :: [Text] -> Text -> Parser Column
    go currList curr =
        either
            fail
            (\(currList', rest') -> do
                -- If symbolCount is less than required amount then the process is done
                let symbolCount = T.length $ T.filter (== '|') rest'
                if T.null rest' || symbolCount < 2
                    then return $ Column currList'
                    else go currList' rest'
            )
            (P.parseOnly (getElem currList) curr)

    getElem :: [Text] -> Parser ([Text], Text)
    getElem currList = do
        _       <- P.char '|'
        element <- T.strip <$> P.takeWhile (/= '|') -- fmmmmm
        rest    <- P.takeText
        return (currList ++ [element], rest)

-- | Convert given common mark table into 'TABLE' block
commonmarkTableToTable :: CommonMarkTable -> Block
commonmarkTableToTable (CommonMarkTable columns) =
    table "table" (map getColumn columns)

-- | Parse given @[Text]@ into 'CommonMarkTable'
parseTable :: [Text] -> Either String Block
parseTable txt = commonmarkTableToTable <$> parseTable' txt
  where
    parseTable' :: [Text] -> Either String CommonMarkTable
    parseTable' texts =
        let header = take 1 texts
            rest   = drop 2 texts
        in go (CommonMarkTable mempty) (header <> rest)
    go :: CommonMarkTable -> [Text] -> Either String CommonMarkTable
    go commonMarkTable []                = return commonMarkTable
    go (CommonMarkTable currList) (t:ts) = do
        column <- P.parseOnly columnParser t
        go (CommonMarkTable (currList <> [column])) ts
