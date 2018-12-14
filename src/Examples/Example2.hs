{-# LANGUAGE OverloadedStrings #-}

module Examples.Example2 where

import           RIO          hiding (link)

import           Constructors (blockQuote, bold, bulletPoint, codeNotation,
                               document, header, italic, lineBreak, link,
                               markdown, noStyle, scrapText, text,
                               strikeThrough, textBlock, thumbnail, codeBlock)
import           Types        (Block(..), Markdown, CodeName(..), CodeSnippet(..))


-- "Syntax",
syntax :: [Block]
syntax = 
    [ document [noStyle [text "Syntax"]]
    , thumbnail "https://gyazo.com/0f82099330f378fe4917a1b4a5fe8815"
    ]

-- "[* Mouse-based editing]",
mouseBased :: [Block]
mouseBased =
    [ header 1 $ [text "Mouse-based editing"]
    , thumbnail "https://gyazo.com/a515ab169b1e371641f7e04bfa92adbc"
    , document 
        [ bold [text "Internal Links"]
        , noStyle [text " (linking to another page on scrapbox)"]
        ]
    ]

internalLinks :: [Block]
internalLinks = 
    [ document 
        [bold 
            [ text "Internal Links"]
            , noStyle [text " (linking to another page on scrapbox)"]
        ]
    , bulletPoint 1 [noStyle [codeNotation "[link]", text " ⇒ ", link Nothing "Link"]]
    ]

externalLinkExample :: [Block]
externalLinkExample =
    [ document 
        [ bold 
            [ text "External Links"]
            , noStyle [text " (linking to another page on scrapbox)"
            ]
        ]
    , bulletPoint 1 
        [ noStyle
            [ codeNotation "http://google.com"
            , text " ⇒ http://google.com"
            ]
        ]
    , document [noStyle [text "or"]]
    , bulletPoint 1 
        [ noStyle 
            [ codeNotation "[Google http://google.com]"
            , text " ⇒ "
            , link (Just "Google") "http://google.com"
            ]
        ]
    ]

directImageLink :: [Block]
directImageLink = 
    [ document $ [bold [text "Images"]]
    , bulletPoint 1 
        [noStyle 
            [ text "Direct mage link ↓"
            , codeNotation "[https://gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]"
            ]
        ]
    , document [noStyle [link Nothing "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png"]]
    ]

clickableThumbnail :: [Block]
clickableThumbnail = 
    [ document [bold [text "Clickable Thumbnail Links"]]
    , document 
        [ noStyle 
            [text "t↓ "
            , codeNotation "`[http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]`"
            , text " "
            ]
        ]
    , bulletPoint 1 [noStyle [link (Just "http://cutedog.com") "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png"]]
    , bulletPoint 1 [noStyle [text "Adding the link at the end also works, as before:"]]
    , bulletPoint 2 [noStyle [codeNotation "[https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png http://cutedog.com]"]]
    ]

linkToOther :: [Block]
linkToOther = 
    [ document [bold [text "Linking to other scrapbox projects"]]
    , bulletPoint 1 
        [noStyle 
            [ codeNotation "[/projectname/pagename]"
            , text " ⇛ "
            , link Nothing "/icons/check"]
            ]
    , bulletPoint 1 
        [noStyle 
            [ codeNotation "[/projectname]"
            , text " ⇛ "
            , link Nothing "/icons"]
            ]
    ]

-- "[[Icons]]",
iconsTitle :: [Block]
iconsTitle = 
    [ document [bold [text "Icons"]]
    , bulletPoint 1 
        [noStyle 
            [ codeNotation "[ben.icon]"
            , text " ⇛  "
            , link Nothing "ben.icon"
            ]
        ]
    , bulletPoint 1 
        [noStyle 
            [ codeNotation "[/icons/todo.icon]"
            , text " ⇛ "
            , link Nothing "/icons/todo.icon"
            ]
        ]
    ]

-- "[[Bold text]]",
boldTitle :: [Block]
boldTitle = 
    [ document [bold [text "Bold text"]]
    , bulletPoint 1
        [ noStyle [codeNotation "[[Bold]]"
        , text " or "
        , codeNotation "[* Bold]"
        , text "⇒ "]
        , bold [text "Bold"]
        ]
    ]

-- "[[Italic text]]",
italicTitle :: [Block]
italicTitle = 
    [ document [bold [text "Italic text"]]
    , bulletPoint 1 
        [ noStyle [codeNotation "[/ italic]", text "⇛ "]
        , italic [text "italic"]
        ]
    ]

-- "[[ Strikethrough text]]",
strikeThroughTitle :: [Block]
strikeThroughTitle = 
    [ document [bold [text " Strikethrough text"]]
    , bulletPoint 1 
        [ noStyle 
            [ codeNotation "[- strikethrough]"
            , text "⇛ "
            ]
        , strikeThrough [text "strikethrough"]
        ]
    , thumbnail "https://gyazo.com/00ab07461d502db91c8ae170276d1396"
    ]

-- "[[Bullet points]]"
bulletPointTitle :: [Block]
bulletPointTitle = 
    [ document [bold [text "Bullet points"]]
    , bulletPoint 1 [noStyle [text "Press space or tab on a new line to indent and create a bullet point"]]
    , bulletPoint 2 [noStyle [text "Press backspace to remove the indent  / bullet point"]]
    ]

-- "[[Hashtags / internal links]]",
hashtagTitle :: [Block]
hashtagTitle = 
    [ document [bold [text "Hashtags / internal links"]]
    , bulletPoint 1 
        [ noStyle 
            [ codeNotation "#tag"
            , text " and " 
            , codeNotation "link"
            , text " work the same to create a link but also define related pages you can find later"
            ]
        ]
    , bulletPoint 1 
        [ noStyle [text "Add links in the middle of a sentence to branch off as you type \
            \or add tags at the end to organize."]
        ]
    ]

-- "[[Block quote]]",
blockQuoteTitle :: [Block]
blockQuoteTitle = 
    [ document [bold [text "Block quote"]]
    , blockQuote 
        [noStyle 
            [ text "> use the right arrow "
            , codeNotation ">"
            , text " at the beginning of a line to get a block quote "
            ]
        ]
    ]

-- "[[[Code notation]]]",
codeNotationTitle :: [Block]
codeNotationTitle = 
    [ document [bold [link Nothing "Code notation"]]
    , bulletPoint 1 [noStyle [text "Use backquotes or backticks, `,  to highlight code  "]]
    , bulletPoint 1 
        [noStyle 
            [ text " e.g. "
            , codeNotation "function() {  return true }"
            ]
        ]
    ]

codeBlockTitle :: [Block]
codeBlockTitle = 
    [ document [bold [link Nothing "[Code block notation]"]]
    , document
        [noStyle
            [ text " Typing "
            , codeNotation "code:filename.extension"
            , text "or"
            , codeNotation "code:filename"
            , text "can be used to create a new code snippet and and display it as a block"
            ]
        ]
    , document [noStyle [text "  Language names may be abbreviated"]]
    ]


-- " code:hello.js",
-- " \tfunction () {",
-- "    alert(document.location.href)",
-- "    console.log(\"hello\")",
-- "    // You can also write comments!",
-- "  }",
-- "",
--     CodeBlock codeName code            -> encodeCodeBlock codeName code
codeContent :: Block
codeContent = codeBlock "hello.js"
    [ "function () {"
    , "   alert(document.location.href)"
    , "   console.log(\"hello\")"
    , "   // You can also write comments!"
    , "}"
    ]

--    [[[Tables]]]
tableTitle :: [Block]
tableTitle = 
    [ document [bold [link Nothing "Tables"]]
    , bulletPoint 1 [noStyle [text "Type table: tablename to create a table"]]
    , bulletPoint 1 [noStyle [text "Use tab to move to the next column, use enter to move to the next row."]]
    , bulletPoint 1 [noStyle [text "An example:"]]
    ]

-- "table:hello",
-- "\t1\t2\t3",
-- "\t1 \t2 \t3",
-- " ------\t------\t------",
-- " a\tb\tc",
-- "",
-- ""

-- "Syntax",
-- "[https://gyazo.com/0f82099330f378fe4917a1b4a5fe8815]",
-- "",
-- "",
-- "[* Mouse-based editing]",
-- "[https://gyazo.com/a515ab169b1e371641f7e04bfa92adbc]",
-- "",
-- "[[Internal Links]] (linking to another page on scrapbox)",
-- "\t`[link]` ⇒ [Link]",
-- "",
-- "[[External  Links]] (linking to another web page)",
-- " `http://google.com` ⇒ http://google.com",
-- "\t`[http://google.com Google]` ⇒ [http://google.com Google]",
-- "or",
-- " `[Google http://google.com]` ⇒ [Google http://google.com]",
-- "",
-- "[[Images]]",
-- "\tDirect mage link ↓`[https://gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]`",
-- " [https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]",
-- "",
-- "[[Clickable Thumbnail Links]]",
-- "\t↓ `[http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]` ",
-- " [http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]",
-- " Adding the link at the end also works, as before:",
-- "  `[https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png http://cutedog.com]`",
-- "",
-- "[[Linking to other scrapbox projects]]",
-- " `[/projectname/pagename]` ⇛ [/icons/check]",
-- " `[/projectname]` ⇛ [/icons]",
-- "",
-- "[[Icons]]",
-- " `[ben.icon]` ⇛  [ben.icon]",
-- " `[/icons/todo.icon]` ⇛ [/icons/todo.icon]",
-- "",
-- "[[Bold text]]",
-- "\t`[[Bold]]` or `[* Bold]`⇒ [[Bold]]",
-- "",
-- "[[Italic text]]",
-- "\t`[/ italic]`⇛ [/ italic]",
-- "",
-- "[[ Strikethrough text]]",
-- " `[- strikethrough]`⇛ [- strikethrough]",
-- "[https://gyazo.com/00ab07461d502db91c8ae170276d1396]",
-- "",
-- "[[Bullet points]]",
-- "\tPress space or tab on a new line to indent and create a bullet point",
-- " \tPress backspace to remove the indent  / bullet point",
-- "",
-- "[[Hashtags / internal links]]",
-- "\t`#tag` and  `[link]` work the same to create a link but also define related pages you can find later",
-- " Add links in the middle of a sentence to branch off as you type or add tags at the end to organize.",
-- "",
-- "[[Block quote]]",
-- "> use the right arrow `>` at the beginning of a line to get a block quote ",
-- "",
-- "[[[Code notation]]]",
-- " Use backquotes or backticks, `,  to highlight code  ",
-- " e.g. `function() {  return true }`",
-- "",
-- "[[[Code block notation]]]",
-- " Typing `code:filename.extension`or`code:filename`can be used to create a new code snippet and and display it as a block",
-- "  Language names may be abbreviated",
-- " code:hello.js",
-- " \tfunction () {",
-- "    alert(document.location.href)",
-- "    console.log(\"hello\")",
-- "    // You can also write comments!",
-- "  }",
-- "",
-- "[[[Tables]]]",
-- "\tType table: tablename to create a table",
-- " Use tab to move to the next column, use enter to move to the next row.",
-- "\tAn example:",
-- "table:hello",
-- "\t1\t2\t3",
-- "\t1 \t2 \t3",
-- " ------\t------\t------",
-- " a\tb\tc",
-- "",
-- ""
