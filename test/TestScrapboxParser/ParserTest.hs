{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- Test suites for scrapbox parser
-}

module TestScrapboxParser.ParserTest
    ( parserSpec
    ) where

import           RIO

import           Data.Scrapbox (Block (..), CodeName (..), CodeSnippet (..),
                                InlineBlock (..), Level (..), ScrapText (..),
                                Scrapbox (..), Segment (..), Start (..),
                                Style (..), TableContent (..), TableName (..),
                                Url (..), renderToScrapbox, size)
import           Data.Scrapbox.Internal (renderBlock, renderScrapText,
                                         renderSegments, runScrapTextParser,
                                         runScrapboxParser, runSpanParser)
import           RIO.List (headMaybe)
import qualified RIO.Text as T
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, label, property, whenFail, within,
                                  (.&&.), (===))
import           Text.Parsec (ParseError)
import           Utils (printDiffs, propNonNull, shouldParseSpec)

-- | Test specs for scrapbox parser
parserSpec :: Spec
parserSpec = do
    scrapboxParserSpec
    spanParserSpec
    scrapTextParserSpec

--------------------------------------------------------------------------------
-- Inline syntax
--------------------------------------------------------------------------------

-- | Spec for inline text parser
spanParserSpec :: Spec
spanParserSpec =
    describe "Span parser" $ modifyMaxSuccess (const 5000) $ do
        shouldParseSpec runSpanParser

        prop "should return non-empty list of segments if given string is non-empty" $
            propNonNull runSpanParser id

        it "should parse given text as expected" $
            propParseAsExpected exampleText expected runSpanParser

        -- Span specs
        describe "Spans" $ modifyMaxSuccess (const 10000) $
            prop "round trip tests" segmentRoundTripTest
  where
    segmentRoundTripTest :: Segment -> Property
    segmentRoundTripTest segment =
        let rendered = renderSegments [segment]
            checkContent = maybe (property False) (=== segment) . headMaybe
        in either
            (const $ property  False)
            checkContent
            (runSpanParser $ T.unpack rendered)

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
-- ScrapText parser
--------------------------------------------------------------------------------

-- | Test spec for scrap text parser
scrapTextParserSpec :: Spec
scrapTextParserSpec =
    describe "ScrapText parser" $ modifyMaxSuccess (const 5000) $ do
        shouldParseSpec runScrapTextParser

        prop "should return non-empty list of contexts if the given string is non-empty" $
            propNonNull runScrapTextParser (\(ScrapText inlines) -> inlines)

        it "should parse given example text as expected" $
             propParseAsExpected exampleText expectedParsedText runScrapTextParser

        describe "Inline blocks" $ modifyMaxSuccess (const 10000) $ 
            prop "round-trip test" scrapTextRoundTripTest

  where
    scrapTextRoundTripTest :: ScrapText -> Property
    scrapTextRoundTripTest scrapText =
        let rendered = renderScrapText scrapText
        in either
            (const $ property  False)
            (=== scrapText)
            (runScrapTextParser $ T.unpack rendered)
    exampleText :: String
    exampleText = "[* bold text] [- strikethrough text] [/ italic text] simple text `code_notation` [* test [link] test [partial]"

    expectedParsedText :: ScrapText
    expectedParsedText = ScrapText
        [ SPAN [ Bold ] [ TEXT "bold text" ]
        , SPAN [] [ TEXT " " ]
        , SPAN [ StrikeThrough ] [ TEXT "strikethrough text" ]
        , SPAN [] [ TEXT " " ]
        , SPAN [ Italic ] [ TEXT "italic text" ]
        , SPAN [] [ TEXT " simple text " ]
        , CODE_NOTATION "code_notation"
        , SPAN [] [ TEXT " " ]
        , SPAN [ Bold ]
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
    describe "Scrapbox parser" $ modifyMaxSuccess (const 5000) $ do
        roundTripSpec
        shouldParseSpec runScrapboxParser

        prop "should be able to perform round-trip on block" blockRoundTripTest
        prop "should return non-empty list of blocks if the given string is non-empty" $
            propNonNull runScrapboxParser (\(Scrapbox blocks) -> blocks)

        syntaxPageTest

-- | Performs roundtrips test
roundTripSpec :: Spec
roundTripSpec = describe "Scrapbox" $
    prop "should be able to perform roundtrip if there's no ambiguous syntax"
        roundTest

roundTest :: Property
roundTest = within 5000000 $ property $
        \(scrapbox :: Scrapbox) ->
              whenFail (printDiffs scrapbox)
            $ label (printSize $ size scrapbox) $
            let rendered = renderToScrapbox mempty scrapbox
                eParsed  = runScrapboxParser $ T.unpack rendered

            in isRight eParsed
            .&&. either
                (const $ property False)
                (=== scrapbox)
                eParsed
  where
    printSize bsize
      | bsize < 5                = "Less than 5"
      | bsize >= 5 && bsize < 10 = "More than 5, but less than 10"
      | otherwise                = "More than 10"

blockRoundTripTest :: Block -> Property
blockRoundTripTest block =
    let rendered = T.unlines . renderBlock $ block
    in either
        (const $ property  False)
        checkContent
        (runScrapboxParser $ T.unpack rendered)
  where
    checkContent =
        maybe
            (property False)
            (=== block)
            . (\(Scrapbox blocks) -> headMaybe blocks)

syntaxPageTest :: Spec
syntaxPageTest =
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

    example5 :: String
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
        [ PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "Syntax" ] ] )
        , THUMBNAIL ( Url "https://gyazo.com/0f82099330f378fe4917a1b4a5fe8815" )
        , LINEBREAK
        , LINEBREAK
        , HEADING ( Level 1 ) [ TEXT "Mouse-based editing" ]
        , THUMBNAIL ( Url "https://gyazo.com/a515ab169b1e371641f7e04bfa92adbc" )
        , LINEBREAK
        , PARAGRAPH
            ( ScrapText
                [ SPAN [ Bold ] [ TEXT "Internal Links" ]
                , SPAN [] [ TEXT " (linking to another page on scrapbox)" ]
                ]
            )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[link]"
                    , SPAN []
                        [ TEXT " ⇒ "
                        , LINK Nothing ( Url "Link" )
                        ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH
            ( ScrapText
                [ SPAN [ Bold ] [ TEXT "External  Links" ]
                , SPAN [] [ TEXT " (linking to another web page)" ]
                ]
            )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "http://google.com"
                    , SPAN [] [ TEXT " ⇒ http://google.com" ]
                    ]
                )
            , PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[http://google.com Google]"
                    , SPAN []
                        [ TEXT " ⇒ "
                        , LINK ( Just "Google" ) ( Url "http://google.com" )
                        ]
                    ]
                )
            ]
        , PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "or" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[Google http://google.com]"
                    , SPAN []
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
        [ PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Images" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ SPAN [] [ TEXT "Direct mage link ↓" ]
                    , CODE_NOTATION "[https://gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]"
                    ]
                )
            , THUMBNAIL ( Url "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png" )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Clickable Thumbnail Links" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ SPAN [] [ TEXT "↓ " ]
                    , CODE_NOTATION "[http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]"
                    , SPAN [] [ TEXT " " ]
                    ]
                )
            , PARAGRAPH ( ScrapText [ SPAN [] [ LINK ( Just "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png" ) ( Url "http://cutedog.com" ) ] ] )
            , PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "Adding the link at the end also works, as before:" ] ] )
            , BULLET_POINT ( Start 1 ) [ PARAGRAPH ( ScrapText [ CODE_NOTATION "[https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png http://cutedog.com]" ] ) ]
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Linking to other scrapbox projects" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/projectname/pagename]"
                    , SPAN []
                        [ TEXT " ⇛ "
                        , LINK Nothing ( Url "/icons/check" )
                        ]
                    ]
                )
            , PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/projectname]"
                    , SPAN []
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
        [ PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Icons" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[ben.icon]"
                    , SPAN []
                        [ TEXT " ⇛  "
                        , LINK Nothing ( Url "ben.icon" )
                        ]
                    ]
                )
            , PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/icons/todo.icon]"
                    , SPAN []
                        [ TEXT " ⇛ "
                        , LINK Nothing ( Url "/icons/todo.icon" )
                        ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Bold text" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[[Bold]]"
                    , SPAN [] [ TEXT " or " ]
                    , CODE_NOTATION "[* Bold]"
                    , SPAN [] [ TEXT "⇒ " ]
                    , SPAN [ Bold ] [ TEXT "Bold" ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Italic text" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/ italic]"
                    , SPAN [] [ TEXT "⇛ " ]
                    , SPAN [ Italic ] [ TEXT "italic" ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT " Strikethrough text" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[- strikethrough]"
                    , SPAN [] [ TEXT "⇛ " ]
                    , SPAN [ StrikeThrough ] [ TEXT "strikethrough" ]
                    ]
                )
            ]
        , THUMBNAIL ( Url "https://gyazo.com/00ab07461d502db91c8ae170276d1396" )
        , LINEBREAK
        ]

    expected4 :: Scrapbox
    expected4 = Scrapbox
        [ PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Bullet points" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "Press space or tab on a new line to indent and create a bullet point" ] ] )
            , BULLET_POINT ( Start 1 ) [ PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "Press backspace to remove the indent  / bullet point" ] ] ) ]
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Hashtags / internal links" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "#tag"
                    , SPAN [] [ TEXT " and  " ]
                    , CODE_NOTATION "[link]"
                    , SPAN [] [ TEXT " work the same to create a link but also define related pages you can find later" ]
                    ]
                )
            , PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "Add links in the middle of a sentence to branch off as you type or add tags at the end to organize." ] ] )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Block quote" ] ] )
        , BLOCK_QUOTE
            ( ScrapText
                [ SPAN [] [ TEXT " use the right arrow " ]
                , CODE_NOTATION ">"
                , SPAN [] [ TEXT " at the beginning of a line to get a block quote " ]
                ]
            )
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Code notation" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "Use backquotes or backticks, `,  to highlight code  " ] ] )
            , PARAGRAPH
                ( ScrapText
                    [ SPAN [] [ TEXT "e.g. " ]
                    , CODE_NOTATION "function() {  return true }"
                    ]
                )
            ]
        , LINEBREAK
        ]


    expected5 :: Scrapbox
    expected5 =  Scrapbox
        [ PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Code block notation" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ SPAN [] [ TEXT "Typing " ]
                    , CODE_NOTATION "code:filename.extension"
                    , SPAN [] [ TEXT "or" ]
                    , CODE_NOTATION "code:filename"
                    , SPAN [] [ TEXT "can be used to create a new code snippet and and display it as a block" ]
                    ]
                )
            , BULLET_POINT ( Start 1 ) [ PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "Language names may be abbreviated" ] ] ) ]
            ]
        , CODE_BLOCK ( CodeName "hello.js" )
            ( CodeSnippet
                [ "function () {"
                , "  alert(document.location.href)"
                , "  console.log(\"hello\")"
                , "  // You can also write comments!"
                , "}"
                ]
            )
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ SPAN [ Bold ] [ TEXT "Tables" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "Type table: tablename to create a table" ] ] )
            , PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "Use tab to move to the next column, use enter to move to the next row." ] ] )
            , PARAGRAPH ( ScrapText [ SPAN [] [ TEXT "An example:" ] ] )
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
        , LINEBREAK
        ]

-- | General unit testing to see the parser can parse given data as expected
propParseAsExpected :: (Eq parsed, Show parsed)
                    => String
                    -> parsed
                    -> (String -> Either ParseError parsed)
                    -> Property
propParseAsExpected example expected parser = property $ either
    (const $ property False)
    (=== expected)
    (parser example)
