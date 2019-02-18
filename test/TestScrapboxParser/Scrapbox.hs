{-| Test suites for 'parseScrapbox'
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestScrapboxParser.Scrapbox
    ( scrapboxParserSpec
    ) where

import           RIO                      hiding (assert)

import qualified RIO.Text                 as T
import           Test.Hspec               (Spec, describe, it)
import           Test.Hspec.QuickCheck    (modifyMaxSuccess, prop)
import           Test.QuickCheck.Monadic  (assert, monadicIO)

import           Parser.Scrapbox          (parseScrapbox)
import           Render                   (renderPretty)
import           TestScrapboxParser.Utils (NonEmptyPrintableString (..),
                                           propParseAsExpected, shouldParseSpec)
import           Types                    (Block (..), CodeName (..),
                                           CodeSnippet (..), InlineBlock (..),
                                           Level (..), ScrapText (..),
                                           Scrapbox (..), Segment (..),
                                           Start (..), Style (..),
                                           TableContent (..), TableName (..),
                                           Url (..))
import           Utils                    (whenRight)
--------------------------------------------------------------------------------
-- Scrapbox parser
--------------------------------------------------------------------------------

scrapboxParserSpec :: Spec
scrapboxParserSpec =
    describe "Scrapbox parser" $ modifyMaxSuccess (const 10000) $ do
        shouldParseSpec parseScrapbox

        prop "should return non-empty list of blocks if the given string is non-empty" $
            \(someText :: NonEmptyPrintableString) -> monadicIO $ do
                let eParseredText = parseScrapbox $ getNonEmptyPrintableString someText

                assert $ isRight eParseredText
                whenRight eParseredText $ \(Scrapbox blocks) ->
                    assert $ not $ null blocks

        describe "Blocks" $ modifyMaxSuccess (const 200) $ 
            blockSpec
    
        describe "Parsing \"syntax\" page with scrapbox parser" $
          modifyMaxSuccess (const 1) $ do
            it "should parse section1 as expected" $
                propParseAsExpected example1 expected1 parseScrapbox

            it "should parse section2 as expected" $
                propParseAsExpected example2 expected2 parseScrapbox

            it "should parse section3 as expected" $
                propParseAsExpected example3 expected3 parseScrapbox

            it "should parse section4 as expected" $
                propParseAsExpected example4 expected4 parseScrapbox

            it "should parse section5 as expected" $
                propParseAsExpected example5 expected5 parseScrapbox
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
        [ PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "Syntax" ] ] )
        , THUMBNAIL ( Url "https://gyazo.com/0f82099330f378fe4917a1b4a5fe8815" )
        , LINEBREAK
        , LINEBREAK
        , HEADING ( Level 1 ) [ TEXT "Mouse-based editing" ]
        , THUMBNAIL ( Url "https://gyazo.com/a515ab169b1e371641f7e04bfa92adbc" )
        , LINEBREAK
        , PARAGRAPH
            ( ScrapText
                [ ITEM Bold [ TEXT "Internal Links" ]
                , ITEM NoStyle [ TEXT " (linking to another page on scrapbox)" ]
                ]
            )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[link]"
                    , ITEM NoStyle
                        [ TEXT " ⇒ "
                        , LINK Nothing ( Url "Link" )
                        ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH
            ( ScrapText
                [ ITEM Bold [ TEXT "External  Links" ]
                , ITEM NoStyle [ TEXT " (linking to another web page)" ]
                ]
            )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "http://google.com"
                    , ITEM NoStyle [ TEXT " ⇒ http://google.com" ]
                    ]
                )
            , PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[http://google.com Google]"
                    , ITEM NoStyle
                        [ TEXT " ⇒ "
                        , LINK ( Just "Google" ) ( Url "http://google.com" )
                        ]
                    ]
                )
            ]
        , PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "or" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[Google http://google.com]"
                    , ITEM NoStyle
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
        [ PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Images" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ ITEM NoStyle [ TEXT "Direct mage link ↓" ]
                    , CODE_NOTATION "[https://gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]"
                    ]
                )
            , THUMBNAIL ( Url "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png" )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Clickable Thumbnail Links" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ ITEM NoStyle [ TEXT "↓ " ]
                    , CODE_NOTATION "[http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]"
                    , ITEM NoStyle [ TEXT " " ]
                    ]
                )
            , PARAGRAPH ( ScrapText [ ITEM NoStyle [ LINK ( Just "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png" ) ( Url "http://cutedog.com" ) ] ] )
            , PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "Adding the link at the end also works, as before:" ] ] )
            , BULLET_POINT ( Start 1 ) [ PARAGRAPH ( ScrapText [ CODE_NOTATION "[https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png http://cutedog.com]" ] ) ]
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Linking to other scrapbox projects" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/projectname/pagename]"
                    , ITEM NoStyle
                        [ TEXT " ⇛ "
                        , LINK Nothing ( Url "/icons/check" )
                        ]
                    ]
                )
            , PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/projectname]"
                    , ITEM NoStyle
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
        [ PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Icons" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[ben.icon]"
                    , ITEM NoStyle
                        [ TEXT " ⇛  "
                        , LINK Nothing ( Url "ben.icon" )
                        ]
                    ]
                )
            , PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/icons/todo.icon]"
                    , ITEM NoStyle
                        [ TEXT " ⇛ "
                        , LINK Nothing ( Url "/icons/todo.icon" )
                        ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Bold text" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[[Bold]]"
                    , ITEM NoStyle [ TEXT " or " ]
                    , CODE_NOTATION "[* Bold]"
                    , ITEM NoStyle [ TEXT "⇒ " ]
                    , ITEM Bold [ TEXT "Bold" ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Italic text" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[/ italic]"
                    , ITEM NoStyle [ TEXT "⇛ " ]
                    , ITEM Italic [ TEXT "italic" ]
                    ]
                )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT " Strikethrough text" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "[- strikethrough]"
                    , ITEM NoStyle [ TEXT "⇛ " ]
                    , ITEM StrikeThrough [ TEXT "strikethrough" ]
                    ]
                )
            ]
        , THUMBNAIL ( Url "https://gyazo.com/00ab07461d502db91c8ae170276d1396" )
        , LINEBREAK
        ]



    expected4 :: Scrapbox
    expected4 = Scrapbox
        [ PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Bullet points" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "Press space or tab on a new line to indent and create a bullet point" ] ] )
            , BULLET_POINT ( Start 1 ) [ PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "Press backspace to remove the indent  / bullet point" ] ] ) ]
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Hashtags / internal links" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ CODE_NOTATION "#tag"
                    , ITEM NoStyle [ TEXT " and  " ]
                    , CODE_NOTATION "[link]"
                    , ITEM NoStyle [ TEXT " work the same to create a link but also define related pages you can find later" ]
                    ]
                )
            , PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "Add links in the middle of a sentence to branch off as you type or add tags at the end to organize." ] ] )
            ]
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Block quote" ] ] )
        , BLOCK_QUOTE
            ( ScrapText
                [ ITEM NoStyle [ TEXT " use the right arrow " ]
                , CODE_NOTATION ">"
                , ITEM NoStyle [ TEXT " at the beginning of a line to get a block quote " ]
                ]
            )
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Code notation" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "Use backquotes or backticks, `,  to highlight code  " ] ] )
            , PARAGRAPH
                ( ScrapText
                    [ ITEM NoStyle [ TEXT "e.g. " ]
                    , CODE_NOTATION "function() {  return true }"
                    ]
                )
            ]
        , LINEBREAK
        ]



    expected5 :: Scrapbox
    expected5 =Scrapbox
        [ PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Code block notation" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH
                ( ScrapText
                    [ ITEM NoStyle [ TEXT "Typing " ]
                    , CODE_NOTATION "code:filename.extension"
                    , ITEM NoStyle [ TEXT "or" ]
                    , CODE_NOTATION "code:filename"
                    , ITEM NoStyle [ TEXT "can be used to create a new code snippet and and display it as a block" ]
                    ]
                )
            , BULLET_POINT ( Start 1 ) [ PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "Language names may be abbreviated" ] ] ) ]
            ]
        , CODE_BLOCK ( CodeName "hello.js" ) ( CodeSnippet 
            [ "function () {"
            , "  alert(document.location.href)"
            , "  console.log(\"hello\")"
            , "  // You can also write comments!"
            , "}"
            ]
        )
        , LINEBREAK
        , PARAGRAPH ( ScrapText [ ITEM Bold [ TEXT "Tables" ] ] )
        , BULLET_POINT ( Start 1 )
            [ PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "Type table: tablename to create a table" ] ] )
            , PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "Use tab to move to the next column, use enter to move to the next row." ] ] )
            , PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "An example:" ] ] )
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

blockSpec :: Spec
blockSpec = describe "Scrapbox" $
    prop "should be able to parse any Scrapbox" $
        \(scrapbox :: Scrapbox) -> monadicIO $ do
            let rendered = renderPretty scrapbox
            let eParsed  = parseScrapbox $ T.unpack rendered

            assert $ isRight eParsed
            
            whenRight eParsed $ \parsed ->
                assert $ parsed == scrapbox

