{-| Utility funcitons used in this Library. They are from extra and either package.

extra: http://hackage.haskell.org/package/extra-1.6.14/docs/Control-Monad-Extra.html

either: http://hackage.haskell.org/package/either-5.0.1
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Utils
    ( -- * Testing utilities
      genPrintableText
    , genText
    , genPrintableUrl
    , genMaybe
    , shortListOf
    ) where

import           RIO

import qualified RIO.Text as T
import           Test.QuickCheck (Gen, elements, listOf1, resize, sized)

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- | Generate arbitrary Text
-- this is needed as some characters like
-- '`' and `>` will be parsed as blockquote, code notation, etc.
genPrintableText :: Gen Text
genPrintableText = T.unwords <$> shortListOf genText

-- | Generate random text
genText :: Gen Text
genText = fmap fromString <$> shortListOf
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

shortListOf :: Gen a -> Gen [a]
shortListOf g = sized $ \s ->
    resize
        ((round :: Double -> Int) . sqrt . fromIntegral $ s)
        (listOf1 (resize s g))
