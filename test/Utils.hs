{-| Utility funcitons used in this Library. They are from extra and either package.

extra: <http://hackage.haskell.org/package/extra-1.6.14/docs/Control-Monad-Extra.html>

-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( whenRight
    , whenJust
    -- * Testing utilities
    , genPrintableText
    , genText
    , genPrintableUrl
    , genMaybe
    , NonEmptyPrintableString(..)
    , shouldParseSpec
    ) where

import           RIO

import qualified RIO.Text as T
import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Gen, PrintableString (..),
                                  arbitraryPrintableChar, elements, listOf1)
import           Text.Parsec (ParseError)

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- | Perform some operation on 'Just', given the field inside the 'Just'.
--
-- > whenJust Nothing  print == return ()
-- > whenJust (Just 1) print == print 1
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

-- | The 'whenRight' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @'Right' _@, otherwise
-- it does nothing.
whenRight :: Applicative m => Either a b -> (b -> m ()) -> m ()
whenRight (Right x) f = f x
whenRight _         _ = pure ()

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
