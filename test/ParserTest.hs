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

import           Types                   (Block (..), CodeName (..),
                                          CodeSnippet (..), Context (..),
                                          Level (..), ScrapText (..),
                                          Scrapbox (..), Segment (..),
                                          Start (..), Style (..),
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
        [ TEXT "hello "
        , LINK ( Just "hello yahoo link" ) ( Url "http://www.yahoo.co.jp" )
        , TEXT " "
        , LINK Nothing ( Url "hello" )
        , TEXT " [] `partial code [partial url "
        , HASHTAG "someHashtag"
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
    exampleText = "[* bold text] [- strikethrough text] [/ italic text] simple text `code_notation` [* test [link] test [partial]"

    expectedParsedText :: ScrapText
    expectedParsedText = ScrapText 
        [ CONTEXT Bold [ TEXT "bold text" ]
        , CONTEXT NoStyle [ TEXT " " ]
        , CONTEXT StrikeThrough [ TEXT "strikethrough text" ]
        , CONTEXT NoStyle [ TEXT " " ]
        , CONTEXT Italic [ TEXT "italic text" ]
        , CONTEXT NoStyle [ TEXT " simple text " ]
        , CODE_NOTATION "code_notation"
        , CONTEXT NoStyle [ TEXT " " ]
        , CONTEXT Bold 
            [ TEXT "test "
            , LINK Nothing ( Url "link" )
            , TEXT " test [partial"
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
                whenRight eParseredText $ \(Scrapbox blocks) ->
                    assert $ not $ null blocks

        describe "Parsing \"syntax\" page with scrapbox parser" $
          modifyMaxSuccess (const 1) $ do
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

    expected1 :: Scrapbox
    expected1 = Scrapbox
        [ PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "Syntax" ] ] )
        , THUMBNAIL ( Url "https://gyazo.com/0f82099330f378fe4917a1b4a5fe8815" )
        , LINEBREAK
        , LINEBREAK
        , HEADING ( Level 1 ) [ TEXT "Mouse-based editing" ]
        , THUMBNAIL ( Url "https://gyazo.com/a515ab169b1e371641f7e04bfa92adbc" )
        , LINEBREAK
        , PARAGRAPH
            ( ScrapText
                [ CONTEXT Bold [ TEXT "Internal Links" ]
                , CONTEXT NoStyle [ TEXT " (linking to another page on scrapbox)" ]
                ]
            )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[link]"
                    , CONTEXT NoStyle
                        [ TEXT " ⇒ "
                        , LINK Nothing ( Url "Link" )
                        ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH
            ( ScrapText
                [ CONTEXT Bold [ TEXT "External  Links" ]
                , CONTEXT NoStyle [ TEXT " (linking to another web page)" ]
                ]
            )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "http://google.com"
                    , CONTEXT NoStyle [ TEXT " ⇒ http://google.com" ]
                    ]
                )
            , PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[http://google.com Google]"
                    , CONTEXT NoStyle
                        [ TEXT " ⇒ "
                        , LINK ( Just "Google" ) ( Url "http://google.com" )
                        ]
                    ]
                )
            ]
        , PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "or" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[Google http://google.com]"
                    , CONTEXT NoStyle
                        [ TEXT " ⇒ "
                        , LINK ( Just "Google" ) ( Url "http://google.com" )
                        ]
                    ]
                )
            ]
        , LINEBREAK
        ]

    expected2 :: Scrapbox
    expected2 = Scrapbox
        [ PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Images" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CONTEXT NoStyle [ TEXT "Direct mage link ↓" ]
                    , CODE_NOTATION "[https://gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]"
                    ]
                )
            , THUMBNAIL ( Url "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png" )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Clickable Thumbnail Links" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CONTEXT NoStyle [ TEXT "↓ " ]
                    , CODE_NOTATION "[http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]"
                    , CONTEXT NoStyle [ TEXT " " ]
                    ]
                )
            , PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ LINK ( Just "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png" ) ( Url "http://cutedog.com" ) ] ] )
            , PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "Adding the link at the end also works, as before:" ] ] )
            , BULLET_POINT ( Start 1 ) [ PARAGRAPH ( ScrapText [ CODE_NOTATION "[https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png http://cutedog.com]" ] ) ]
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Linking to other scrapbox projects" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/projectname/pagename]"
                    , CONTEXT NoStyle
                        [ TEXT " ⇛ "
                        , LINK Nothing ( Url "/icons/check" )
                        ]
                    ]
                )
            , PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/projectname]"
                    , CONTEXT NoStyle
                        [ TEXT " ⇛ "
                        , LINK Nothing ( Url "/icons" )
                        ]
                    ]
                )
            ]
        , LINEBREAK
        ]


    expected3 :: Scrapbox
    expected3 = Scrapbox
        [ PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Icons" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[ben.icon]"
                    , CONTEXT NoStyle
                        [ TEXT " ⇛  "
                        , LINK Nothing ( Url "ben.icon" )
                        ]
                    ]
                )
            , PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/icons/todo.icon]"
                    , CONTEXT NoStyle
                        [ TEXT " ⇛ "
                        , LINK Nothing ( Url "/icons/todo.icon" )
                        ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Bold text" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[[Bold]]"
                    , CONTEXT NoStyle [ TEXT " or " ]
                    , CODE_NOTATION "[* Bold]"
                    , CONTEXT NoStyle [ TEXT "⇒ " ]
                    , CONTEXT Bold [ TEXT "Bold" ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Italic text" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/ italic]"
                    , CONTEXT NoStyle [ TEXT "⇛ " ]
                    , CONTEXT Italic [ TEXT "italic" ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT " Strikethrough text" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[- strikethrough]"
                    , CONTEXT NoStyle [ TEXT "⇛ " ]
                    , CONTEXT StrikeThrough [ TEXT "strikethrough" ]
                    ]
                )
            ]
        , THUMBNAIL ( Url "https://gyazo.com/00ab07461d502db91c8ae170276d1396" )
        , LINEBREAK
        ]



    expected4 :: Scrapbox
    expected4 = Scrapbox
        [ PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Bullet points" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "Press space or tab on a new line to indent and create a bullet point" ] ] )
            , BULLET_POINT ( Start 1 ) [ PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "Press backspace to remove the indent  / bullet point" ] ] ) ]
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Hashtags / internal links" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "#tag"
                    , CONTEXT NoStyle [ TEXT " and  " ]
                    , CODE_NOTATION "[link]"
                    , CONTEXT NoStyle [ TEXT " work the same to create a link but also define related pages you can find later" ]
                    ]
                )
            , PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "Add links in the middle of a sentence to branch off as you type or add tags at the end to organize." ] ] )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Block quote" ] ] )
        , BLOCK_QUOTE
            ( ScrapText
                [ CONTEXT NoStyle [ TEXT " use the right arrow " ]
                , CODE_NOTATION ">"
                , CONTEXT NoStyle [ TEXT " at the beginning of a line to get a block quote " ]
                ]
            )
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Code notation" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "Use backquotes or backticks, `,  to highlight code  " ] ] )
            , PARAGRAPH
                ( ScrapText
                    [ CONTEXT NoStyle [ TEXT "e.g. " ]
                    , CODE_NOTATION "function() {  return true }"
                    ]
                )
            ]
        , LINEBREAK
        ]



    expected5 :: Scrapbox
    expected5 =Scrapbox
        [ PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Code block notation" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CONTEXT NoStyle [ TEXT "Typing " ]
                    , CODE_NOTATION "code:filename.extension"
                    , CONTEXT NoStyle [ TEXT "or" ]
                    , CODE_NOTATION "code:filename"
                    , CONTEXT NoStyle [ TEXT "can be used to create a new code snippet and and display it as a block" ]
                    ]
                )
            , BULLET_POINT ( Start 1 ) [ PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "Language names may be abbreviated" ] ] ) ]
            ]
        , CODE_BLOCK ( CodeName "hello.js" ) ( CodeSnippet (T.unlines
            [ "function () {"
            , "  alert(document.location.href)"
            , "  console.log(\"hello\")"
            , "  // You can also write comments!"
            , "}"
            ])
        )
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ CONTEXT Bold [ TEXT "Tables" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "Type table: tablename to create a table" ] ] )
            , PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "Use tab to move to the next column, use enter to move to the next row." ] ] )
            , PARAGRAPH ( ScrapText [ CONTEXT NoStyle [ TEXT "An example:" ] ] )
            ]
        , TABLE ( TableName "hello" )
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
        , LINEBREAK
        ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

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
