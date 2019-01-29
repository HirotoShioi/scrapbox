{- Test suites for scrapbox parser
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParserTest
    ( parserSpec
    ) where

import           RIO                     hiding (assert)
import qualified RIO.Text                as T

import           Test.Hspec              (Spec, describe, it)
import           Test.Hspec.QuickCheck   (modifyMaxSuccess, prop)
import           Test.QuickCheck         (Arbitrary (..), PrintableString (..),
                                          Property, arbitraryPrintableChar,
                                          listOf1)
import           Test.QuickCheck.Monadic (assert, monadicIO)
import           Text.Parsec             (ParseError)

import           Parser.Inline           (runInlineParser)
import           Parser.Scrapbox         (runScrapboxParser)
import           Parser.Text             (runScrapTextParser)

import           Types                   (Block (..), BulletSize (..),
                                          CodeName (..), CodeSnippet (..),
                                          Context (..), HeaderSize (..),
                                          Markdown (..), ScrapText (..),
                                          Segment (..), Style (..),
                                          TableContent (..), TableName (..),
                                          Url (..))
import           Utils                   (eitherM, whenRight)

-- | Test specs for scrapbox parser
parserSpec :: Spec
parserSpec = do
    inlineParserSpec
    scrapTextParserSpec
    scrapboxParserSpec


-- | Spec for inline text parser
inlineParserSpec :: Spec
inlineParserSpec =
    describe "Inline text parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runInlineParser

        prop "should return non-empty list of segments if given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runInlineParser $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \parsedContent ->
                    assert $ not $ null parsedContent

        it "should parse given text as expected" $
            propParseAsExpected exampleText expected runInlineParser
  where
    exampleText :: String
    exampleText = "hello [hello yahoo link http://www.yahoo.co.jp] [hello] [] `partial code [partial url #someHashtag"

    expected :: [Segment]
    expected =
        [ SimpleText "hello "
        , Link ( Just "hello yahoo link" ) ( Url "http://www.yahoo.co.jp" )
        , SimpleText " "
        , Link Nothing ( Url "hello" )
        , SimpleText " [] `partial code [partial url "
        , HashTag "someHashtag"
        ]

-- | Test spec for scrap text parser
scrapTextParserSpec :: Spec
scrapTextParserSpec =
    describe "ScrapText parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runScrapTextParser

        prop "should return non-empty list of contexts if the given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runScrapTextParser $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \(ScrapText ctxs) ->
                    assert $ not $ null ctxs

        it "should parse given example text as expected" $
             propParseAsExpected exampleText expectedParsedText runScrapTextParser
  where
    exampleText :: String
    exampleText = "[* bold text] [- strikethrough text] [/ italic text] simple text [* test [link] test [partial]"

    expectedParsedText :: ScrapText
    expectedParsedText = ScrapText
            [ Context Bold [ SimpleText "bold text" ]
            , Context NoStyle [ SimpleText " " ]
            , Context StrikeThrough [ SimpleText "strikethrough text" ]
            , Context NoStyle [ SimpleText " " ]
            , Context Italic [ SimpleText "italic text" ]
            , Context NoStyle [ SimpleText " simple text " ]
            , Context Bold
                [ SimpleText "test "
                , Link Nothing ( Url "link" )
                , SimpleText " test [partial"
                ]
            ]

--------------------------------------------------------------------------------
-- Scrapbox parser
--------------------------------------------------------------------------------

scrapboxParserSpec :: Spec
scrapboxParserSpec =
    describe "Scrapbox parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec runScrapboxParser

        prop "should return non-empty list of blocks if the given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = runScrapboxParser $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \(Markdown blocks) ->
                    assert $ not $ null blocks
    
        describe "Parsing getting syntax page with scrapbox parser" $ modifyMaxSuccess (const 1) $ do
            it "should parse section1 as expected" $
                propParseAsExpected example1 expected1 runScrapboxParser
            
            it "should parse section2 as expected" $
                propParseAsExpected example2 expected2 runScrapboxParser

            it "should parse section3 as expected" $
                propParseAsExpected example3 expected3 runScrapboxParser

            it "should parse section4 as expected" $
                propParseAsExpected example4 expected4 runScrapboxParser

            it "should parse section5 as expected" $
                propParseAsExpected example5 expected5 runScrapboxParser
  where
    example1 :: String
    example1 = unlines [
        "Syntax",
        "[https://gyazo.com/0f82099330f378fe4917a1b4a5fe8815]",
        "",
        "",
        "[* Mouse-based editing]",
        "[https://gyazo.com/a515ab169b1e371641f7e04bfa92adbc]",
        "",
        "[[Internal Links]] (linking to another page on scrapbox)",
        "\t`[link]` ⇒ [Link]",
        "",
        "[[External  Links]] (linking to another web page)",
        " `http://google.com` ⇒ http://google.com",
        "\t`[http://google.com Google]` ⇒ [http://google.com Google]",
        "or",
        " `[Google http://google.com]` ⇒ [Google http://google.com]",
        ""
        ]

    example2 = unlines [
        "[[Images]]",
        "\tDirect mage link ↓`[https://gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]`",
        " [https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]",
        "",
        "[[Clickable Thumbnail Links]]",
        "\t↓ `[http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]` ",
        " [http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]",
        " Adding the link at the end also works, as before:",
        "  `[https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png http://cutedog.com]`",
        "",
        "[[Linking to other scrapbox projects]]",
        " `[/projectname/pagename]` ⇛ [/icons/check]",
        " `[/projectname]` ⇛ [/icons]",
        ""
        ]

    example3 = unlines [
        "[[Icons]]",
        " `[ben.icon]` ⇛  [ben.icon]",
        " `[/icons/todo.icon]` ⇛ [/icons/todo.icon]",
        "",
        "[[Bold text]]",
        "\t`[[Bold]]` or `[* Bold]`⇒ [[Bold]]",
        "",
        "[[Italic text]]",
        "\t`[/ italic]`⇛ [/ italic]",
        "",
        "[[ Strikethrough text]]",
        " `[- strikethrough]`⇛ [- strikethrough]",
        "[https://gyazo.com/00ab07461d502db91c8ae170276d1396]",
        ""
        ]
    
    example4 = unlines [
        "[[Bullet points]]",
        "\tPress space or tab on a new line to indent and create a bullet point",
        " \tPress backspace to remove the indent  / bullet point",
        "",
        "[[Hashtags / internal links]]",
        "\t`#tag` and  `[link]` work the same to create a link but also define related pages you can find later",
        " Add links in the middle of a sentence to branch off as you type or add tags at the end to organize.",
        "",
        "[[Block quote]]",
        "> use the right arrow `>` at the beginning of a line to get a block quote ",
        "",
        "[[Code notation]]",
        " Use backquotes or backticks, `,  to highlight code  ",
        " e.g. `function() {  return true }`",
        ""
        ]

    example5 = unlines [
        "[[Code block notation]]",
        " Typing `code:filename.extension`or`code:filename`can be used to create a new code snippet and and display it as a block",
        "  Language names may be abbreviated",
        "code:hello.js",
        " function () {",
        "   alert(document.location.href)",
        "   console.log(\"hello\")",
        "   // You can also write comments!",
        " }",
        "",
        "[[Tables]]",
        "\tType table: tablename to create a table",
        " Use tab to move to the next column, use enter to move to the next row.",
        "\tAn example:",
        "table:hello",
        "\t1\t2\t3",
        "\t1 \t2 \t3",
        " ------\t------\t------",
        " a\tb\tc",
        "",
        ""
        ]

    expected1 :: Markdown
    expected1 = Markdown 
        [ Paragraph ( ScrapText [ Context NoStyle [ SimpleText "Syntax" ] ] )
        , Thumbnail ( Url "https://gyazo.com/0f82099330f378fe4917a1b4a5fe8815" )
        , LineBreak
        , LineBreak
        , Header ( HeaderSize 1 ) [ SimpleText "Mouse-based editing" ]
        , Thumbnail ( Url "https://gyazo.com/a515ab169b1e371641f7e04bfa92adbc" )
        , LineBreak
        , Paragraph 
            ( ScrapText 
                [ Context Bold [ SimpleText "Internal Links" ]
                , Context NoStyle [ SimpleText " (linking to another page on scrapbox)" ]
                ] 
            )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "[link]"
                    , SimpleText " ⇒ "
                    , Link Nothing ( Url "Link" )
                    ] 
                ]
            )
        , LineBreak
        , Paragraph 
            ( ScrapText 
                [ Context Bold [ SimpleText "External  Links" ]
                , Context NoStyle [ SimpleText " (linking to another web page)" ]
                ] 
            )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "http://google.com"
                    , SimpleText " ⇒ http://google.com"
                    ] 
                ]
            )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "[http://google.com Google]"
                    , SimpleText " ⇒ "
                    , Link ( Just "Google" ) ( Url "http://google.com" )
                    ] 
                ]
            )
        , Paragraph ( ScrapText [ Context NoStyle [ SimpleText "or" ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "[Google http://google.com]"
                    , SimpleText " ⇒ "
                    , Link ( Just "Google" ) ( Url "http://google.com" )
                    ] 
                ]
            )
        , LineBreak
        ]

    expected2 :: Markdown
    expected2 = Markdown
        [ Paragraph ( ScrapText [ Context Bold [ SimpleText "Images" ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ SimpleText "Direct mage link ↓"
                    , CodeNotation "[https://gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]"
                    ] 
                ]
            )
        , BulletPoint ( BulletSize 1 ) ( ScrapText [ Context NoStyle [ Link Nothing ( Url "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png" ) ] ] )
        , LineBreak
        , Paragraph ( ScrapText [ Context Bold [ SimpleText "Clickable Thumbnail Links" ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ SimpleText "↓ "
                    , CodeNotation "[http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]"
                    , SimpleText " "
                    ] 
                ]
            )
        , BulletPoint ( BulletSize 1 ) ( ScrapText [ Context NoStyle [ Link ( Just "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png" ) ( Url "http://cutedog.com" ) ] ] )
        , BulletPoint ( BulletSize 1 ) ( ScrapText [ Context NoStyle [ SimpleText "Adding the link at the end also works, as before:" ] ] )
        , BulletPoint ( BulletSize 2 ) ( ScrapText [ Context NoStyle [ CodeNotation "[https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png http://cutedog.com]" ] ] )
        , LineBreak
        , Paragraph ( ScrapText [ Context Bold [ SimpleText "Linking to other scrapbox projects" ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "[/projectname/pagename]"
                    , SimpleText " ⇛ "
                    , Link Nothing ( Url "/icons/check" )
                    ] 
                ]
            )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "[/projectname]"
                    , SimpleText " ⇛ "
                    , Link Nothing ( Url "/icons" )
                    ] 
                ]
            )
        , LineBreak
        ]
    
    expected3 :: Markdown
    expected3 = Markdown
        [ Paragraph ( ScrapText [ Context Bold [ SimpleText "Icons" ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "[ben.icon]"
                    , SimpleText " ⇛  "
                    , Link Nothing ( Url "ben.icon" )
                    ] 
                ]
            )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "[/icons/todo.icon]"
                    , SimpleText " ⇛ "
                    , Link Nothing ( Url "/icons/todo.icon" )
                    ] 
                ]
            )
        , LineBreak
        , Paragraph ( ScrapText [ Context Bold [ SimpleText "Bold text" ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "[[Bold]]"
                    , SimpleText " or "
                    , CodeNotation "[* Bold]"
                    , SimpleText "⇒ "
                    ] 
                , Context Bold [ SimpleText "Bold" ]
                ] 
            )
        , LineBreak
        , Paragraph ( ScrapText [ Context Bold [ SimpleText "Italic text" ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "[/ italic]"
                    , SimpleText "⇛ "
                    ] 
                , Context Italic [ SimpleText "italic" ]
                ] 
            )
        , LineBreak
        , Paragraph ( ScrapText [ Context Bold [ SimpleText " Strikethrough text" ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "[- strikethrough]"
                    , SimpleText "⇛ "
                    ] 
                , Context StrikeThrough [ SimpleText "strikethrough" ]
                ] 
            )
        , Thumbnail ( Url "https://gyazo.com/00ab07461d502db91c8ae170276d1396" )
        , LineBreak
        ]
    
    expected4 :: Markdown
    expected4 = Markdown
        [ Paragraph ( ScrapText [ Context Bold [ SimpleText "Bullet points" ] ] )
        , BulletPoint ( BulletSize 1 ) ( ScrapText [ Context NoStyle [ SimpleText "Press space or tab on a new line to indent and create a bullet point" ] ] )
        , BulletPoint ( BulletSize 2 ) ( ScrapText [ Context NoStyle [ SimpleText "Press backspace to remove the indent  / bullet point" ] ] )
        , LineBreak
        , Paragraph ( ScrapText [ Context Bold [ SimpleText "Hashtags / internal links" ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ CodeNotation "#tag"
                    , SimpleText " and  "
                    , CodeNotation "[link]"
                    , SimpleText " work the same to create a link but also define related pages you can find later"
                    ] 
                ]
            )
        , BulletPoint ( BulletSize 1 ) ( ScrapText [ Context NoStyle [ SimpleText "Add links in the middle of a sentence to branch off as you type or add tags at the end to organize." ] ] )
        , LineBreak
        , Paragraph ( ScrapText [ Context Bold [ SimpleText "Block quote" ] ] )
        , BlockQuote 
            ( ScrapText 
                [ Context NoStyle 
                    [ SimpleText " use the right arrow "
                    , CodeNotation ">"
                    , SimpleText " at the beginning of a line to get a block quote "
                    ] 
                ]
            )
        , LineBreak
        , Paragraph ( ScrapText [ Context Bold [ SimpleText "Code notation" ] ] )
        , BulletPoint ( BulletSize 1 ) ( ScrapText [ Context NoStyle [ SimpleText "Use backquotes or backticks, `,  to highlight code  " ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ SimpleText "e.g. "
                    , CodeNotation "function() {  return true }"
                    ] 
                ]
            )
        , LineBreak
        ]

    expected5 :: Markdown
    expected5 = Markdown    
        [ Paragraph ( ScrapText [ Context Bold [ SimpleText "Code block notation" ] ] )
        , BulletPoint ( BulletSize 1 ) 
            ( ScrapText 
                [ Context NoStyle 
                    [ SimpleText "Typing "
                    , CodeNotation "code:filename.extension"
                    , SimpleText "or"
                    , CodeNotation "code:filename"
                    , SimpleText "can be used to create a new code snippet and and display it as a block"
                    ] 
                ]
            )
        , BulletPoint ( BulletSize 2 ) ( ScrapText [ Context NoStyle [ SimpleText "Language names may be abbreviated" ] ] )
        , CodeBlock ( CodeName "hello.js" ) ( CodeSnippet (T.unlines
            [ "function () {"
            , "  alert(document.location.href)"
            , "  console.log(\"hello\")"
            , "  // You can also write comments!"
            , "}"
            ])
        )
        , LineBreak
        , Paragraph ( ScrapText [ Context Bold [ SimpleText "Tables" ] ] )
        , BulletPoint ( BulletSize 1 ) ( ScrapText [ Context NoStyle [ SimpleText "Type table: tablename to create a table" ] ] )
        , BulletPoint ( BulletSize 1 ) ( ScrapText [ Context NoStyle [ SimpleText "Use tab to move to the next column, use enter to move to the next row." ] ] )
        , BulletPoint ( BulletSize 1 ) ( ScrapText [ Context NoStyle [ SimpleText "An example:" ] ] )
        , Table ( TableName "hello" ) 
            ( TableContent 
                [ 
                    [ "1"
                    , "2"
                    , "3"
                    ] 
                , 
                    [ "1 "
                    , "2 "
                    , "3"
                    ] 
                , 
                    [ "------"
                    , "------"
                    , "------"
                    ] 
                , 
                    [ "a"
                    , "b"
                    , "c"
                    ] 
                ] 
            )
        , LineBreak
        ]


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Non-empty version of 'PrintableString'
newtype NonEmptyPrintableString =  NonEmptyPrintableString {
    getNonEmptyPrintableString :: String
    } deriving Show

instance Arbitrary NonEmptyPrintableString where
    arbitrary = NonEmptyPrintableString <$> listOf1 arbitraryPrintableChar

-- | General testing spec for parser
shouldParseSpec :: (String -> Either ParseError a) -> Spec
shouldParseSpec parser =
        prop "should be able to parse any text without failing or cause infinite loop" $
            \(someText :: PrintableString) ->
                isRight $ parser $ getPrintableString someText

-- | General unit testing to see the parser can parse given data as expected
propParseAsExpected :: (Eq parsed)
                    => toParse
                    -> parsed
                    -> (toParse -> Either ParseError parsed)
                    -> Property
propParseAsExpected example expected parser = monadicIO $ eitherM
    (\parseError    -> fail $ "Failed to parse with error: " <> show parseError)
    (\parsedContent -> assert $ parsedContent == expected)
    (return $ parser example)
