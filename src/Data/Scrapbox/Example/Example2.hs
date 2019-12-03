{-| Example of how our defined data structure can be used to represent the scrapbox page

Example page: https://scrapbox.io/toSrapbox/Syntax
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Scrapbox.Example.Example2
    ( example2'
    ) where

import           RIO hiding (link, span)

import           Data.Scrapbox.Constructors (blockQuote, bold, bulletPoint,
                                             codeBlock, codeNotation, heading,
                                             italic, lineBreak, link, p,
                                             scrapbox, span, strikeThrough,
                                             table, text, thumbnail)
import           Data.Scrapbox.Types (Block (..), Scrapbox)


syntax :: [Block]
syntax =
    [ p [ span [] [text "Syntax"]]
    , thumbnail "https://gyazo.com/0f82099330f378fe4917a1b4a5fe8815"
    ]

-- "[* Mouse-based editing]",
-- "[https://gyazo.com/a515ab169b1e371641f7e04bfa92adbc]",
mouseBased :: [Block]
mouseBased =
    [ heading 1 [text "Mouse-based editing"]
    , thumbnail "https://gyazo.com/a515ab169b1e371641f7e04bfa92adbc"
    , p
        [ span [bold] [text "Internal Links"]
        , span [] [text " (linking to another page on scrapbox)"]
        ]
    ]

-- "[[Internal Links]] (linking to another page on scrapbox)",
-- "\t`[link]` ⇒ [Link]
internalLinks :: [Block]
internalLinks =
    [ p
        [ span [bold] [text "Internal Links"]
        , span [] [text " (linking to another page on scrapbox)"]
        ]
    , bulletPoint 1
        [ p
            [ codeNotation "[link]"
            , span []
                [ text " ⇒ "
                , link Nothing "Link"
                ]
            ]
        ]
    ]

-- "[[External  Links]] (linking to another web page)",
-- " `http://google.com` ⇒ http://google.com",
-- "\t`[http://google.com Google]` ⇒ [http://google.com Google]",
-- "or",
-- " `[Google http://google.com]` ⇒ [Google http://google.com]",
externalLinks :: [Block]
externalLinks =
    [ p
        [ span [bold] [text "External Links"]
        , span [] [text " (linking to another page on scrapbox)"]
        ]
    , bulletPoint 1
        [ p
            [ codeNotation "http://google.com"
            , span [] [text " ⇒ http://google.com"]
            ]
        ]
    , p [span [] [text "or"]]
    , bulletPoint 1
        [ p
            [ codeNotation "[Google http://google.com]"
            , span []
                [ text " ⇒ "
                , link (Just "Google") "http://google.com"
                ]
            ]
        ]
    ]

-- "[[Images]]",
-- "\tDirect mage link ↓`[https://gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]`",
-- " [https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]",
directImageLink :: [Block]
directImageLink =
    [ p [ span [] [text "Images"]]
    , bulletPoint 1
        [ p
            [ span [] [ text "Direct mage link ↓" ]
            , codeNotation "[https://gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]"
            ]
        ]
    , p [ span [] [link Nothing "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png"]]
    ]

-- "[[Clickable Thumbnail Links]]",
-- "\t↓ `[http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]` ",
-- " [http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]",
-- " Adding the link at the end also works, as before:",
-- "  `[https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png http://cutedog.com]`",
clickableThumbnail :: [Block]
clickableThumbnail =
    [ p [span [bold] [text "Clickable Thumbnail Links"]]
    , p
        [ span [] [text "t↓ "]
        , codeNotation "`[http://cutedog.com https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png]`"
        , span [] [text " "]
        ]
    , bulletPoint 1 [ p [span [] [link (Just "http://cutedog.com") "https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png"]]]
    , bulletPoint 1 [ p [span [] [text "Adding the link at the end also works, as before:"]]]
    , bulletPoint 2 [ p [codeNotation "[https://i.gyazo.com/da78df293f9e83a74b5402411e2f2e01.png http://cutedog.com]"]]
    ]

-- "[[Linking to other scrapbox projects]]",
-- " `[/projectname/pagename]` ⇛ [/icons/check]",
-- " `[/projectname]` ⇛ [/icons]",
linkToOther :: [Block]
linkToOther =
    [ p [ span [bold] [text "Linking to other scrapbox projects"]]
    , bulletPoint 1
        [ p
            [ codeNotation "[/projectname/pagename]"
            , span []
                [ text " ⇛ "
                , link Nothing "/icons/check"
                ]
            ]
        ]
    , bulletPoint 1
        [ p
            [ codeNotation "[/projectname]"
            , span []
                [ text " ⇛ "
                , link Nothing "/icons"
                ]
            ]
        ]
    ]

-- "[[Icons]]",
-- " `[ben.icon]` ⇛  [ben.icon]",
-- " `[/icons/todo.icon]` ⇛ [/icons/todo.icon]",
iconSection :: [Block]
iconSection =
    [ p [span [bold] [text "Icons"]]
    , bulletPoint 1
        [ p
            [ codeNotation "[ben.icon]"
            , span []
                [ text " ⇛  "
                , link Nothing "ben.icon"
                ]
            ]
        ]
    , bulletPoint 1
        [ p
            [ codeNotation "[/icons/todo.icon]"
            , span []
                [ text " ⇛ "
                , link Nothing "/icons/todo.icon"
                ]
            ]
        ]
    ]

-- "[[Bold text]]",
-- "\t`[[Bold]]` or `[* Bold]`⇒ [[Bold]]",
boldSection :: [Block]
boldSection =
    [ p [span [bold] [text "Bold text"]]
    , bulletPoint 1
        [ p
            [ codeNotation "[[Bold]]"
            , span [] [text " or "]
            , codeNotation "[* Bold]"
            , span [] [text "⇒ "]
            , span [bold] [text "Bold"]
            ]
        ]
    ]

-- "[[Italic text]]",
-- "\t`[/ italic]`⇛ [/ italic]",
italicSection :: [Block]
italicSection =
    [ p [span [bold] [text "Italic text"]]
    , bulletPoint 1
        [ p
            [ codeNotation "[/ italic]"
            , span [] [text "⇛ "]
            , span [italic] [text "italic"]
            ]
        ]
    ]

-- "[[ Strikethrough text]]",
-- " `[- strikethrough]`⇛ [- strikethrough]",
-- "[https://gyazo.com/00ab07461d502db91c8ae170276d1396]",
strikeThroughSection :: [Block]
strikeThroughSection =
    [ p [span [bold] [text " Strikethrough text"]]
    , bulletPoint 1
        [ p
            [ codeNotation "[- strikethrough]"
            , span [] [text "⇛ "]
            , span [strikeThrough] [text "strikethrough"]
            ]
        ]
    , thumbnail "https://gyazo.com/00ab07461d502db91c8ae170276d1396"
    ]

-- "[[Bullet points]]",
-- "\tPress space or tab on a new line to indent and create a bullet point",
-- " \tPress backspace to remove the indent  / bullet point",
bulletPointSection :: [Block]
bulletPointSection =
    [ p [span [bold] [text "Bullet points"]]
    , bulletPoint 1
        [ p [span [] [text "Press space or tab on a new line to indent and create a bullet point"]]
        ]
    , bulletPoint 2
        [ p [span [] [text "Press backspace to remove the indent  / bullet point"]]
        ]
    ]

-- "[[Hashtags / internal links]]",
-- "\t`#tag` and  `[link]` work the same to create a link but also define related pages you can find later",
-- " Add links in the middle of a sentence to branch off as you type or add tags at the end to organize.",
hashtagSection :: [Block]
hashtagSection =
    [ p [ span [bold] [text "Hashtags / internal links"]]
    , bulletPoint 1
        [ p
            [ codeNotation "#tag"
            , span [] [ text " and "]
            , codeNotation "link"
            , span [] [ text " work the same to create a link but also define related pages you can find later"]
            ]
        ]
    , bulletPoint 1
        [ p
            [ span [] [text "Add links in the middle of a sentence to branch off as you type \
                \or add tags at the end to organize."]
            ]
        ]
    ]

-- "[[Block quote]]",
-- "> use the right arrow `>` at the beginning of a line to get a block quote ",
blockQuoteSection :: [Block]
blockQuoteSection =
    [ p [span [bold] [text "Block quote"]]
    , blockQuote
        [ span [] [text "> use the right arrow "]
        , codeNotation ">"
        , span [] [text " at the beginning of a line to get a block quote "]
        ]
    ]

-- "[[[Code notation]]]",
-- " Use backquotes or backticks, `,  to highlight code  ",
-- " e.g. `function() {  return true }`",
codeNotationSection :: [Block]
codeNotationSection =
    [ p [span [bold] [link Nothing "Code notation"]]
    , bulletPoint 1
        [ p [ span [] [text "Use backquotes or backticks, `,  to highlight code  "]]
        ]
    , bulletPoint 1
        [ p
            [ span [] [ text " e.g. "]
            , codeNotation "function() {  return true }"
            ]
        ]
    ]

-- "[[[Code block notation]]]",
-- " Typing `code:filename.extension`or`code:filename`can be used to create a new code snippet and and display it as a block",
-- "  Language names may be abbreviated",
codeBlockSection :: [Block]
codeBlockSection =
    [ p [span [bold] [link Nothing "[Code block notation]"]]
    , p
        [ span [] [text " Typing "]
        , codeNotation "code:filename.extension"
        , span [] [text "or"]
        , codeNotation "code:filename"
        , span [] [text "can be used to create a new code snippet and and display it as a block"]
        ]
    , p [ span [] [text "  Language names may be abbreviated"]]
    , codeContent
    ]
  where
    -- " code:hello.js",
    -- " \tfunction () {",
    -- "    alert(p.location.href)",
    -- "    console.log(\"hello\")",
    -- "    // You can also write comments!",
    -- "  }",
    -- "",
    --     CodeBlock codeName code            -> encodeCodeBlock codeName code
    codeContent :: Block
    codeContent = codeBlock "hello.js"
        [ "function () {"
        , "   alert(p.location.href)"
        , "   console.log(\"hello\")"
        , "   // You can also write comments!"
        , "}"
        ]

-- "[[[Tables]]]",
-- "\tType table: tablename to create a table",
-- " Use tab to move to the next column, use enter to move to the next row.",
-- "\tAn example:",
-- "table:hello",
-- "\t1\t2\t3",
-- "\t1 \t2 \t3",
-- " ------\t------\t------",
-- " a\tb\tc",
tableSection :: [Block]
tableSection =
    [ p [ span [bold] [link Nothing "Tables"]]
    , bulletPoint 1 [ p [ span [] [text "Type table: tablename to create a table"]]]
    , bulletPoint 1 [ p [ span [] [text "Use tab to move to the next column, use enter to move to the next row."]]]
    , bulletPoint 1 [ p [ span [] [text "An example:"]]]
    , tableExample
    ]
  where
    -- "table:hello",
    -- "\t1\t2\t3",
    -- "\t1 \t2 \t3",
    -- " ------\t------\t------",
    -- " a\tb\tc",
    -- "",
    -- ""
    tableExample :: Block
    tableExample = table "hello"
        [ ["1", "2", "3"]
        , ["1", "2", "3"]
        , ["------", "------", "------"]
        , ["a","b", "c"]
        ]

section :: [Block] -> [Block]
section bs = bs <> [lineBreak]

-- | Example Page
-- https://scrapbox.io/toSrapbox/Syntax
example2' :: Scrapbox
example2' = scrapbox $ concatMap section
    [ syntax
    , mouseBased
    , internalLinks
    , externalLinks
    , directImageLink
    , clickableThumbnail
    , linkToOther
    , iconSection
    , boldSection
    , italicSection
    , strikeThroughSection
    , bulletPointSection
    , hashtagSection
    , blockQuoteSection
    , codeNotationSection
    , codeBlockSection
    , tableSection
    ]

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
-- "    alert(p.location.href)",
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
