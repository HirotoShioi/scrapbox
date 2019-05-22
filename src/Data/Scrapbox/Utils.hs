{-| Utility funcitons used in this Library.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Utils
    ( -- * Testing utilities
      genPrintableText
    , genNonSpaceText
    , genPrintableUrl
    , genMaybe
    , shortListOf
    , isURL
    ) where

import           RIO

import           Data.Char (isSpace)
import           RIO.List (headMaybe, isPrefixOf, stripPrefix)
import qualified RIO.Text as T
import           Test.QuickCheck (Gen, elements, listOf1, resize, sized)
import           Test.QuickCheck.Arbitrary (arbitraryPrintableChar)
import           Test.QuickCheck.Gen (suchThat)

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- | Generate arbitrary Text
-- this is needed as some characters like
-- '`' and `>` will be parsed as blockquote, code notation, etc.
genPrintableText :: Gen Text
genPrintableText = fromString <$>
        listOf1 (arbitraryPrintableChar
            `suchThat`
            (`notElem` syntaxSymobls)
        )
        `suchThat`
        hasNoTrailingSpaces

-- | Generate text without whitespaces
genNonSpaceText :: Gen Text
genNonSpaceText = fromString <$>
    listOf1 (arbitraryPrintableChar
        `suchThat`
        (\c -> c `notElem` syntaxSymobls && (not . isSpace) c)
    )

hasNoTrailingSpaces :: String -> Bool
hasNoTrailingSpaces str =
    let txt = T.pack str
    in T.strip txt == txt

syntaxSymobls :: String
syntaxSymobls = ['*', '[', ']', '/', '\\', '$', '#', '"', '\'', '`', '>']

-- | Generate random url
genPrintableUrl :: Gen Text
genPrintableUrl = do
    end <- elements 
        [ ".org"
        , ".edu"
        , ".com"
        , ".co.jp"
        , ".io"
        , ".tv"
        , ".jpg"
        , ".jpeg"
        , ".gif"
        , ".png"
        ]
    randomSite <- genUrlText
    return $ "http://www." <> randomSite <> end

genUrlText :: Gen Text
genUrlText = fmap fromString <$> listOf1
    $ elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])

-- | Wrap 'Gen a' with 'Maybe'
genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen = do
    gened <- gen
    elements [Just gened, Nothing]

-- | Generate shorter list
shortListOf :: Gen a -> Gen [a]
shortListOf g = sized $ \s ->
    resize
        ((round :: Double -> Int) . sqrt . fromIntegral $ s)
        (listOf1 (resize s g))

-- | Checks if given text is URL
isURL :: String -> Bool
isURL str =
       ("http://" `isPrefixOf` str && hasSomeChar "http://")
    || ("https://" `isPrefixOf` str && hasSomeChar "https://")
  where
    hasSomeChar pre = maybe False (not . isSpace) (stripPrefix pre str >>= headMaybe)
