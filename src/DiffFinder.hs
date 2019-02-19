module DiffFinder where

import           RIO
import qualified RIO.Text        as T

import           Parser.Scrapbox
import           Render
import           Types

data DiffPair = DiffPair
    { original :: !Block
    , parsed   :: !Block
    } deriving Show

findDiffs :: Scrapbox -> Either String [DiffPair]
findDiffs sb@(Scrapbox blocks) = do
    let eParsed = parseScrapbox . T.unpack $ renderPretty sb
    case eParsed of
        Left _                        -> Left "Failed"
        Right (Scrapbox parsedBlocks) -> return $ diffs blocks parsedBlocks
  where
    diffs :: [Block] -> [Block] -> [DiffPair]
    diffs = go mempty

    go :: [DiffPair] -> [Block] -> [Block] -> [DiffPair]
    go diffPair [] _          = diffPair
    go diffPair _ []          = diffPair
    go diffPair (BULLET_POINT _ blocks1: xs) (BULLET_POINT _ blocks2: ys) =
        let diffs' = go diffPair blocks1 blocks2
        in go diffs' xs ys
    go diffPair (x:xs) (y:ys)
        | x == y    = go diffPair xs ys
        | otherwise = go (DiffPair x y : diffPair) xs ys
