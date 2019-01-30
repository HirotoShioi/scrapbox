{-# LANGUAGE OverloadedStrings #-}

module Main where

import           RIO

import           CMark

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

exampleText :: String
exampleText = unlines [
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
    "",
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
    "",
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
    "",
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
    "",
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