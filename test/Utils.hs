{-| Utility funcitons used in this Library. They are from extra and either package.

extra: <http://hackage.haskell.org/package/extra-1.6.14/docs/Control-Monad-Extra.html>

-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( genPrintableUrl
    , genMaybe
    , genNoSymbolText
    , NonEmptyPrintableString(..)
    , shouldParseSpec
    , propNonNull
    , findDiffs
    , DiffPair(..)
    , Syntax(..)
    ) where

import           RIO

import           Data.Char
import           Data.Scrapbox
import           Data.Scrapbox.Internal
import qualified RIO.Text as T
import           Test.Hspec (Spec, it)
import           Test.QuickCheck (Arbitrary (..), Gen, PrintableString (..),
                                  Property, arbitraryPrintableChar, elements,
                                  listOf1, property, suchThat, within, (.&&.))
import           Text.Parsec (ParseError)

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- | Type class used to render/get content of given syntax
class Syntax a where
    render     :: a -> Text

-- | Generate random text
genNoSymbolText :: Gen Text
genNoSymbolText = fromString <$> listOf1 (arbitraryPrintableChar `suchThat` isLetter)

-- | Generate random url
genPrintableUrl :: Gen Text
genPrintableUrl = do
    end        <- elements [".org", ".edu", ".com", ".co.jp", ".io", ".tv"]
    randomSite <- genNoSymbolText
    return $ "http://www." <> randomSite <> end

-- | Wrap 'Gen a' with 'Maybe'
genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen = do
    gened <- gen
    elements [Just gened, Nothing]

-- | Non-empty version of 'PrintableString'
newtype NonEmptyPrintableString =  NonEmptyPrintableString
    { getNonEmptyPrintableString :: String
    } deriving Show

instance Arbitrary NonEmptyPrintableString where
    arbitrary = NonEmptyPrintableString <$> listOf1 arbitraryPrintableChar

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
