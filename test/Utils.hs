{-| Utility funcitons used for testing.
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( genNoSymbolText
    , NonEmptyPrintableString(..)
    , shouldParseSpec
    , propNonNull
    , findDiffs
    , printDiffs
    , DiffPair(..)
    ) where

import           RIO

import           Data.Char (isLetter)
import           Data.Scrapbox (Block (..), Scrapbox (..), renderToScrapbox)
import           Data.Scrapbox.Internal (runScrapboxParser)
import           Prelude (putStrLn)
import qualified RIO.Text as T
import           Test.Hspec (Spec, it)
import           Test.QuickCheck (Arbitrary (..), Gen, PrintableString (..),
                                  Property, arbitraryPrintableChar, listOf1,
                                  property, suchThat, within, (.&&.), sized, resize)
import           Text.Parsec (ParseError)

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- | Generate random text
genNoSymbolText :: Gen Text
genNoSymbolText = fromString <$> listOf1 (arbitraryPrintableChar `suchThat` isLetter)

-- | Non-empty version of 'PrintableString'
newtype NonEmptyPrintableString =  NonEmptyPrintableString
    { getNonEmptyPrintableString :: String
    } deriving Show

instance Arbitrary NonEmptyPrintableString where
    arbitrary = sized $ \s -> do
        someText <- resize (s * 50) $ listOf1 arbitraryPrintableChar 
        return $ NonEmptyPrintableString someText

-- | General testing spec for parser
shouldParseSpec :: (String -> Either ParseError a) -> Spec
shouldParseSpec parser =
        it "should be able to parse any text without failing or cause infinite loop" $
            within 5000000 $ property $ \(someText :: PrintableString) ->
                isRight $ parser $ getPrintableString someText

-- | General test case on whether given parser returns non null list after parsing
-- Non null string
propNonNull :: (String -> Either ParseError a)
            -- ^ Parser
            -> (a -> [b])
            -- ^ Getter
            -> Property
propNonNull parser getter = property $ \(someText :: NonEmptyPrintableString) ->
    let eParseredText = parser
            $ getNonEmptyPrintableString someText

    in isRight eParseredText
    .&&. either
        (const False)
        (not . null . getter)
        eParseredText


data DiffPair = DiffPair
    { original :: !Block
    -- ^ Original block data
    , parsed   :: !Block
    -- ^ Parsed data
    } deriving Show

-- | Perform a roundtrip (render given 'Scrapbox' then parsing it) then compares
-- two block data.
findDiffs :: Scrapbox -> Either String [DiffPair]
findDiffs sb@(Scrapbox blocks) =
    either
        (const $ Left "Failed to parse")
        (\(Scrapbox parsedBlocks) -> return $ diffs blocks parsedBlocks)
        (runScrapboxParser . T.unpack $ renderToScrapbox [] sb)
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

printDiffs :: Scrapbox -> IO ()
printDiffs sb = do
    let diffs = findDiffs sb
    case diffs of
        Left str       -> putStrLn str
        Right diffPairs -> do
            putStrLn "Original:"
            putStrLn $ show sb
            forM_ diffPairs (\(DiffPair before after) -> do
                putStrLn "Before:"
                putStrLn $ show before
                putStrLn "After:"
                putStrLn $ show after
                )
