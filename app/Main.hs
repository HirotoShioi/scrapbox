{-# LANGUAGE OverloadedStrings #-}

module Main where

import           RIO

import           CMark (Node, commonmarkToNode, optHardBreaks, optSafe)
import           Types
--------------------------------------------------------------------------------
-- Test files
--------------------------------------------------------------------------------

-- | Test data for example.md
test :: IO Node
test = testWith "./docs/example.md"

-- | Test data for Header
testHeader :: IO Node
testHeader = testWith "./docs/headers.md"

-- | Test data for nested List
testNestedList :: IO Node
testNestedList = testWith "./docs/nestedList.md"

-- | Test table
testTable :: IO Node
testTable = testWith "./docs/table.md"

testWith :: FilePath -> IO Node
testWith filePath = do
    markDown <- readFileUtf8 filePath
    let options = [optSafe, optHardBreaks]
    let parsed = commonmarkToNode options markDown
    return parsed

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

example2 :: String
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

example3 :: String
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

example4 :: String
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

main :: IO ()
main = undefined

problem :: Scrapbox
problem =Scrapbox 
    [ BULLET_POINT ( Start 2 ) 
        [ BLOCK_QUOTE 
            ( ScrapText 
                [ ITEM Italic 
                    [ TEXT "iqE"
                    , LINK ( Just "n O37Ac 3pyL jAp Vid" ) ( Url "http://www.u0I.tv" )
                    , LINK ( Just "KhT CFi niK D QNHn" ) ( Url "http://www.s.com" )
                    , LINK ( Just "E5q2 v" ) ( Url "http://www.4A.com" )
                    ] 
                , ITEM NoStyle [ TEXT "6 JXF d5M mJW Etem4WjWau" ]
                ] 
            )
        , PARAGRAPH 
            ( ScrapText 
                [ ITEM NoStyle 
                    [ LINK ( Just "jDG s xJFH" ) ( Url "http://www.44B.co.jp" )
                    , TEXT "j ZHj b"
                    , LINK Nothing ( Url "http://www.N.io" )
                    ] 
                , CODE_NOTATION "Ohg cBV1t T 74"
                , CODE_NOTATION "S iub"
                , CODE_NOTATION "y6 t nGTc w"
                , ITEM NoStyle [ TEXT "6 cR40In t 3G i5aK hKe" ]
                ] 
            )
        , PARAGRAPH ( ScrapText [ CODE_NOTATION "Dj6bK PWc9l xQ M Hp" ] )
        ] 
    , BULLET_POINT ( Start 3 ) 
        [ CODE_BLOCK ( CodeName "c4o" ) 
            ( CodeSnippet 
                [ "GmX"
                , "DZ5bE 19 1w dJLTJ"
                , "t re M0Z"
                , "T7e"
                ] 
            )
        , LINEBREAK
        , PARAGRAPH 
            ( ScrapText 
                [ MATH_EXPRESSION "q"
                , ITEM NoStyle 
                    [ LINK ( Just "h72ha mfIOH u" ) ( Url "http://www.htVcG.edu" )
                    , HASHTAG "W8"
                    , TEXT "  t ujbmd"
                    ] 
                , ITEM Bold 
                    [ LINK ( Just "cR Db 1IYl jB" ) ( Url "http://www.ak.org" )
                    , TEXT "jGHLy 3QnNi C1 54P2 Dg US aeOBV mFlow"
                    ] 
                , ITEM Italic 
                    [ LINK ( Just "Wbiy jN" ) ( Url "http://www.BD.org" )
                    , TEXT "C9h62 oM DgvO Xt GQn"
                    , HASHTAG "h"
                    , TEXT "  tr jNd2S ALaB"
                    ] 
                ] 
            )
        , TABLE ( TableName "Ivk" ) 
            ( TableContent 
                [ 
                    [ "R"
                    , "Hlkc"
                    , "tMb"
                    , "4"
                    ] 
                , 
                    [ "tWD"
                    , "XGO"
                    , "v"
                    , "7"
                    ] 
                , 
                    [ "nu"
                    , "efM"
                    , "d"
                    , "lLCc"
                    ] 
                , 
                    [ "M"
                    , "3"
                    , "R8"
                    , "k"
                    ] 
                ] 
            )
        , LINEBREAK
        , CODE_BLOCK ( CodeName "Brtk" ) ( CodeSnippet [ "tdO HqLwE xMNJG bE" ] )
        ] 
    , THUMBNAIL ( Url "http://www.FGdO.edu" )
    , LINEBREAK
    ] 