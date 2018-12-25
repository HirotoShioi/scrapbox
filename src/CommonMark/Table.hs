{-# LANGUAGE OverloadedStrings #-}

module CommonMark.Table 
    ( commonTableToTable
    , parseTable
    ) where

import           RIO

import           Data.Attoparsec.Text      (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Prelude              (String)

import Types

data CommonTable = CommonTable [Column]
    deriving Show

data Column = Column 
    { getColumn :: [Text]
    } deriving (Show)

columnParser :: Parser Column
columnParser = do
    rest <- P.takeText
    go mempty rest
  where
    go :: [Text] -> Text -> Parser Column
    go currList curr =
        either
            (\err -> fail err)
            (\(currList', rest') -> do
                let symbolCount = T.length $ T.filter (== '|') rest'
                if T.null rest' || symbolCount < 2
                    then return $ Column currList'
                    else go currList' rest'
            )
            (P.parseOnly (getElem currList) curr)

    getElem :: [Text] -> Parser ([Text], Text)
    getElem currList = do
        _       <- P.char '|'
        element <- T.strip <$> P.takeWhile (/= '|')
        rest    <- P.takeText
        return $ ((currList ++ [element]), rest)

parseTable :: [Text] -> Either String CommonTable
parseTable texts =
    let header = take 1 texts
        rest   = drop 2 texts
    in go (CommonTable mempty) (header <> rest)
  where
    go :: CommonTable -> [Text] -> Either String CommonTable
    go table []                      = return table
    go (CommonTable currList) (t:ts) =
        either
            (\a -> Left a) -- ????????
            (\column -> go (CommonTable (currList <> [column])) ts)
            (P.parseOnly columnParser t)

commonTableToTable :: CommonTable -> Block
commonTableToTable (CommonTable columns) = 
    Table (TableName "table") (TableContent (map getColumn columns))
