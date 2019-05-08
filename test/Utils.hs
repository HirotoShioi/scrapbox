{-| Utility funcitons used in this Library. They are from extra and either package.

extra: <http://hackage.haskell.org/package/extra-1.6.14/docs/Control-Monad-Extra.html>

-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( -- * Testing utilities
      genPrintableText
    , genText
    , genPrintableUrl
    , genMaybe
    , NonEmptyPrintableString(..)
    , shouldParseSpec
    , propNonNull
    ) where

import           RIO

import qualified RIO.Text as T
import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Gen, PrintableString (..),
                                  Property, arbitraryPrintableChar, elements,
                                  listOf1, property, (.&&.))
import           Text.Parsec (ParseError)

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- | Generate arbitrary Text
-- this is needed as some characters like
-- '`' and `>` will be parsed as blockquote, code notation, etc.
genPrintableText :: Gen Text
genPrintableText = T.unwords <$> listOf1 genText

-- | Generate random text
genText :: Gen Text
genText = fmap fromString <$> listOf1
    $ elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])

-- | Generate random url
genPrintableUrl :: Gen Text
genPrintableUrl = do
    end        <- elements [".org", ".edu", ".com", ".co.jp", ".io", ".tv"]
    randomSite <- genText
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
        prop "should be able to parse any text without failing or cause infinite loop" $
            \(someText :: PrintableString) ->
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
