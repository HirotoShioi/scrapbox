{-| Test suites for 'runItemParser'
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.Inline
    ( inlineParserSpec
    ) where

import           RIO                      hiding (assert)

import           RIO.List                 (headMaybe)
import qualified RIO.Text                 as T
import           Test.Hspec               (Spec, describe, it)
import           Test.Hspec.QuickCheck    (modifyMaxSuccess, prop)
import           Test.QuickCheck          (Arbitrary (..), Gen, elements,
                                           listOf1)
import           Test.QuickCheck.Monadic  (assert, monadicIO)

import           Scrapbox.Internal        (runItemParser)
import           Scrapbox.Types           (Segment (..), Url (..), isHashTag,
                                           isLink, isText)
import           TestScrapboxParser.Utils (NonEmptyPrintableString (..),
                                           ScrapboxSyntax (..), checkContent,
                                           checkParsed, propParseAsExpected,
                                           shouldParseSpec)
-- | Spec for inline text parser
inlineParserSpec :: Spec
inlineParserSpec =
    describe "Item parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runItemParser

        prop "should return non-empty list of segments if given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runItemParser $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \parsedContent ->
                    assert $ not $ null parsedContent

        it "should parse given text as expected" $
            propParseAsExpected exampleText expected runItemParser

        -- Item specs
        describe "Items" $ modifyMaxSuccess (const 200) $ do
            textSpec
            linkSpec
            hashTagSpec
  where
    exampleText :: String
    exampleText = "hello [hello yahoo link http://www.yahoo.co.jp] [hello] [] `partial code [partial url #someHashtag"

    expected :: [Segment]
    expected =
        [ TEXT "hello "
        , LINK ( Just "hello yahoo link" ) ( Url "http://www.yahoo.co.jp" )
        , TEXT " "
        , LINK Nothing ( Url "hello" )
        , TEXT " [] `partial code [partial url "
        , HASHTAG "someHashtag"
        ]

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

newtype TextItem = TextItem Text
    deriving Show

instance Arbitrary TextItem where
    arbitrary = TextItem <$> genPrintableText

instance ScrapboxSyntax TextItem where
    render (TextItem txt)     = txt
    getContent (TextItem txt) = txt


--- Text
textSpec :: Spec
textSpec = describe "TEXT" $ do
    prop "should parse text as TEXT" $
        \(someText :: TextItem) ->
            checkParsed someText runItemParser headMaybe isText
    prop "should preserve its content" $
        \(someText ::TextItem) ->
            checkContent someText runItemParser
                (\segments -> do
                  guard $ length segments == 1
                  segment <- headMaybe segments
                  getText segment
                )
  where
    getText :: Segment -> Maybe Text
    getText (TEXT text) = Just text
    getText _           = Nothing

--------------------------------------------------------------------------------
-- Link
--------------------------------------------------------------------------------

data LinkItem = LinkItem !(Maybe Text) !Text
    deriving Show

instance Arbitrary LinkItem where
    arbitrary = LinkItem <$> genMaybe genPrintableText <*> genPrintableUrl

instance ScrapboxSyntax LinkItem where
    render (LinkItem (Just name) url) = "[" <> name <> " " <> url <> "]"
    render (LinkItem Nothing url)     = "[" <> url <> "]"
    getContent (LinkItem mName url)   = fromMaybe mempty mName <> url

linkSpec :: Spec
linkSpec = describe "LINK" $ do
    prop "should parse link as LINK" $
        \(linkItem :: LinkItem) ->
            checkParsed linkItem runItemParser headMaybe isLink
    prop "should preserve its content" $
        \(linkItem :: LinkItem) -> checkContent linkItem runItemParser
            (\segments -> do
                guard $ length segments == 1
                segment <- headMaybe segments
                (LINK mName (Url url)) <- getLink segment
                return $ fromMaybe mempty mName <> url
            )
  where
    getLink :: Segment -> Maybe Segment
    getLink l@(LINK _ _) = Just l
    getLink _            = Nothing

--------------------------------------------------------------------------------
-- HashTag
--------------------------------------------------------------------------------

newtype HashTagItem = HashTagItem Text
    deriving Show

instance Arbitrary HashTagItem where
    arbitrary = HashTagItem <$> genText

instance ScrapboxSyntax HashTagItem where
    render (HashTagItem text)     = "#" <> text
    getContent (HashTagItem text) = text

hashTagSpec :: Spec
hashTagSpec = describe "HASHTAG" $ do
    prop "should parse hashtag as HASHTAG" $
        \(hashTag :: HashTagItem) ->
            checkParsed hashTag runItemParser headMaybe isHashTag

    prop "should preserve its content" $
        \(hashTag :: HashTagItem) ->
            checkContent hashTag runItemParser
                (\segments -> do
                    guard $ length segments == 1
                    segment       <- headMaybe segments
                    (HASHTAG txt) <- getHashTag segment
                    return txt
                )
  where
    getHashTag :: Segment -> Maybe Segment
    getHashTag h@(HASHTAG _) = Just h
    getHashTag _             = Nothing

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
