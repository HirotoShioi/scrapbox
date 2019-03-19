{-| This module is intended to be used on diagnosing the failing round trip test.
'findDiff' will perform an roundtrip on given 'Scrapbox' and returns diff between
the original data and the parsed data.
-}
module Scrapbox.DiffFinder
    ( findDiffs
    ) where

import           RIO
import qualified RIO.Text                 as T

import           Scrapbox.Parser.Scrapbox (runScrapboxParser)
import           Scrapbox.Render.Scrapbox (renderToScrapbox)
import           Scrapbox.Types           (Block (..), Scrapbox (..))

data DiffPair = DiffPair
    { original :: !Block
    -- ^ Original block data
    , parsed   :: !Block
    -- ^ Parsed data
    } deriving Show

-- | Perform a roundtrip (render given 'Scrapbox' then parsing it) then compares two
-- block data.
findDiffs :: Scrapbox -> Either String [DiffPair]
findDiffs sb@(Scrapbox blocks) =
    either
        (\_ -> Left "Failed to parse")
        (\(Scrapbox parsedBlocks) -> return $ diffs blocks parsedBlocks)
        (runScrapboxParser . T.unpack $ renderToScrapbox sb)
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
