{-| Example of how our defined parse tree can be used to represent the scrapbox page

Example page: https://scrapbox.io/scrapbox-parser/Get_started 
-}

{-# LANGUAGE OverloadedStrings #-}

module Examples.Example1
  ( getStartedMarkdown
  , getsInterestingMd
  , onceStartedMd
  , getStartedMd
  ) where

import           RIO          hiding (link)

import           Constructors (blockQuote, bold, bulletPoint, codeNotation,
                               header, italic, lineBreak, link, markdown,
                               noStyle, p, strikeThrough, text, thumbnail)
import           Types        (Block, Markdown)

--------------------------------------------------------------------------------
-- SECTION: Get started
--------------------------------------------------------------------------------

-- "Get started",
getStarted :: Block
getStarted = p [noStyle [text "Get started"]]

-- "[https://gyazo.com/5f93e65a3b979ae5333aca4f32600611]",
startedThumbnail :: Block
startedThumbnail = thumbnail "https://gyazo.com/5f93e65a3b979ae5333aca4f32600611"

-- "Welcome to your new Scrapbox project!",
welcome :: Block
welcome = p [noStyle [text "Welcome to your new Scrapbox project!"]]

-- "[** 📝Everything is editable]"
everythingIs :: Block
everythingIs = header 2 [text "📝 Everything is editable"]

-- "\tClick on any line and start typing to edit. "
clickOn :: Block
clickOn = bulletPoint 1 
    [ p
        [noStyle [text "Click on any line and start typing to edit. "]]
    ]

-- "\t\tPress tab at the beginning of a line to indent and add a bullet point."
pressTab :: Block
pressTab = bulletPoint 2 
    [ p
        [noStyle [text "Press tab at the beginning of a line to indent and add a bullet point."]]
    ]

-- " Highlight text to make it a [new link], [* bold], [- and] [/ more]."
highlightText :: Block
highlightText = bulletPoint 1 
    [ p
        [ noStyle
            [ text " Highlight text to make it a "
            , link Nothing "New link"
            , text ", "
            ]
        , bold [text "bold"]
        , noStyle [text ", "]
        , strikeThrough [text "and"]
        , noStyle [text " "]
        , italic [text "more"]
        , noStyle [text "."]
        ]
    ]

-- "\t\tAdd links while typing with a `#` before or brackets around `[`words you want to link `]` "
addLinks :: Block
addLinks = bulletPoint 2 
    [ p
        [ noStyle
            [ text "Add links while typing with a "
            , codeNotation "#"
            , text " before or brackets around "
            , codeNotation "["
            , text "words you want to link "
            , codeNotation "]"
            ]
        ]
    ]

----------------------------------------------------------------------------------------------------
-- SECTION: Here is where it gets interesting
----------------------------------------------------------------------------------------------------

-- "[** 🎯 Here is where it gets interesting ]",
hereIs :: Block
hereIs = header 2 [text "🎯 Here is where it gets interesting "]

-- "\tClick a [new link] to create a new page with that title and open it.",
clickNewLink :: Block
clickNewLink =
    bulletPoint 1 
        [ p
            [ noStyle
                [ text "Click a "
                , link Nothing "New Link"
                , text " to create a new page with that title and open it."
                ]
            ]
        ]

-- Click related thumbnails in the footer of any page to explore ideas you have linked.
clickRelated :: Block
clickRelated = p [noStyle [text "Click related thumbnails in the footer of any page to explore ideas \
    \you have linked." ]]

-- " \tPages that are directly linked or two steps away from the current page will be displayed.",
pagesThat :: Block
pagesThat = bulletPoint 2 
    [ p
        [ noStyle 
            [text "Pages that are directly linked or two steps away from the current page \
            \will be displayed."
            ]
        ]
    ]

-- "\tSee images, videos, and external links added inside `[` brackets`]` on the page",
seeImages :: Block
seeImages = bulletPoint 1 
    [ p
        [ noStyle
            [ text "See images, videos, and external links added inside "
            , codeNotation "["
            , text " brackets"
            , codeNotation  "]"
            , text " on the page"
            ]
        ]
    ]

-- "> Our goal is to help you build a map of your ideas that gains clarity and context with every scrap you add. ",
ourGoalIs :: Block
ourGoalIs = blockQuote [noStyle [ourGoalItext]]
  where
    ourGoalItext = text " Our goal is to help you build a map of your ideas that gains\
    \ clarity and context with every scrap you add. "

----------------------------------------------------------------------------------------------------
-- SECTION: What can you put in a Scrapbox project?
----------------------------------------------------------------------------------------------------

-- "[* What can you put in a Scrapbox project?]",
whatCan :: Block
whatCan = header 1 [text "What can you put in a Scrapbox project?"]

-- "\tUse Scrapbox to outline ideas, discuss `code blocks`, give feedback, and brainstorm. ",
useScrapbox :: Block
useScrapbox = bulletPoint 1
    [ p
        [noStyle
            [ text "Use Scrapbox to outline ideas, discuss "
            , codeNotation "code blocks"
            , text ", give feedback, and brainstorm. "
            ]
        ]
    ]

-- "[* For example]",
forExample :: Block
forExample = bulletPoint 1 [p [noStyle [text "For example"]]]

-- "\tLets say you are working on developing a new website. You might want to discuss ideas with
-- your team before and while you execute the plan.  First create a page `Site plan` to start a
-- conversation about the site requirements and link some useful resources. On that page you might
-- add a link for a new page called `Social media buttons`.",
letsSay :: Block
letsSay = bulletPoint 1 [ p [ noStyle [letsSayText, sitePlan, toStart, socialMedia, period]]]
  where
    letsSayText = text "Lets say you are working on developing a new website. \
    \You might want to discuss ideas with your team before and while you execute the plan.  First create a page "
    sitePlan    = codeNotation "Site plan"
    toStart     = text " to start a conversation about the site requirements and link \
    \some useful resources. On that page you might add a link for a new page called "
    socialMedia = codeNotation "Social media buttons"
    period      = text "."

-- "\tYou can immediately click on that link to `Social media buttons` and start editing.
-- There you may add links to `Twitter`, `Facebook`, etc.  Next you can click on `Twitter` and you'll
-- see a related link that will take you back to `Site Plan`. ",
youCanImmediately :: Block
youCanImmediately = bulletPoint 1
    [ p
        [noStyle
            [ youcan, socialMedia, andStart, twitter, column, faceBook, nextYoucan
            , twitter, relatedLink, sitePlan, period
            ]
        ]
    ]
  where
    youcan      = text "You can immediately click on that link to `Social media buttons` and \
    \ start editing. There you may add links to "
    socialMedia = codeNotation "Social media buttons"
    twitter     = codeNotation "Twitter"
    faceBook    = codeNotation "Facebook"
    andStart    = text " and start editing.  There you may add links to "
    column      = text ", "
    nextYoucan  = text ", etc.  Next you can click on "
    relatedLink = text " and you'll see a related link that will take you back to "
    sitePlan    = codeNotation "Site Plan"
    period      = text ". "

-- Once you can easily and directly type your ideas while also building context ideas become more
-- clear the more you use it. No more folders full of dead text means no more teams isolated from their own ideas.
onceYoucan :: Block
onceYoucan = p [noStyle [text "Once you can easily and directly type your ideas while also building \
\ context ideas become more clear the more you use it. No more folders full of dead text means no \
\more teams isolated from their own ideas."]]

-- >  [/ What ideas in your head could your team benefit from you putting down right now? Go create
--  your first three or so pages and add a few links. From 3 to 3,000 pages your ideas will only grow in context.]
whatIdeas :: Block
whatIdeas = blockQuote [italic [text "What ideas in your head could your team \
    \ benefit from you putting down right now? Go create your first three or so pages and add a few \
    \links. From 3 to 3,000 pages your ideas will only grow in context."]]

-- "[** 📌 Once you've got the basics, here are ways to dig deeper and get the most out of your new project ]",
onceYouGot :: Block
onceYouGot = header 2 [text "📌 Once you've got the basics, here are ways to dig deeper and \
  \get the most out of your new project "]


-- " See a list of all the [https://scrapbox.io/help/Things%20you%20can%20do Things you can do] ",
seeAList :: Block
seeAList = p
    [ noStyle
        [ text " See a list of all the "
        , link (Just "Things you can do") "https://scrapbox.io/help/Things%20you%20can%20do"
        , text " "
        ]
    ]

-- " \tIncludes more syntax, inviting team members, and creating profiles",
includesMore :: Block
includesMore = bulletPoint 1
    [ p
        [ noStyle 
            [ text "Includes more syntax, inviting team members, and creating profiles"]
        ]
    ]

-- "\tSee some [https://scrapbox.io/help/examples Example projects] ",
seeSome :: Block
seeSome = bulletPoint 1
    [ p
        [ noStyle
            [ text "See some "
            , link (Just "Example projects") "https://scrapbox.io/help/exampless"
            , text " "
            ]
        ]
    ]

-- " \tIncludes a SaaS startup, design agency, and more",
includesSaas :: Block
includesSaas = bulletPoint 2 
    [ p 
        [ noStyle
            [text "Includes a SaaS startup, design agency, and more"]
        ]
    ]


-- "\tSee [https://scrapbox.io/help/ How-tos and support] ",
howTos :: Block
howTos = bulletPoint 1 
    [ p
        [noStyle
            [ text "See "
            , link (Just "How-tos and support") "https://scrapbox.io/help/"
            , text " "
            ]
        ]
    ]

-- " \tFor detailed instructions and answers to FAQs",
forDetails :: Block
forDetails = bulletPoint 2 
    [ p 
        [noStyle [text "For detailed instructions and answers to FAQs"]]
    ]

-- "[* We would love to hear any questions or feedback you may have]",
weWouldLove :: Block
weWouldLove = header 1 [text "We would love to hear any questions or feedback you may have"]

-- "Please let us know if you have any suggestions, questions, or points of friction.You can contact us directly by email: contact@scrapbox.io, [twitter https://twitter.com/scrapboxapp], and [https://facebook.com/scrapboxapp facebook]",
pleaseLet :: Block
pleaseLet = p
    [noStyle
        [ pleaseLetText
        , link (Just "twitter") "https://twitter.com/scrapboxapp"
        , text ", and "
        , link (Just "facebook") "https://facebook.com/scrapboxapp"
        ]
    ]
  where
    pleaseLetText = text "Please let us know if you have any suggestions, questions, or \
      \points of friction.You can contact us directly by email: contact@scrapbox.io, "

-- "[/ Thank you for using Scrapbox!]",
thankYouFor :: Block
thankYouFor = p [italic [text "Thank you for using Scrapbox!"]]

-- "[https://gyazo.com/5aeffb3e8a6561ae78430664d8257f58]",
thankYouThumbnail :: Block
thankYouThumbnail = thumbnail "https://gyazo.com/5aeffb3e8a6561ae78430664d8257f58"

-- ">Note: When you're done reading you might change the title of this page to 'Welcome to
-- project-name' and add some personalized instructions for your team.",
noteWhen :: Block
noteWhen = blockQuote [noStyle [noteWhenText]]
  where
    noteWhenText =  text "Note: When you're done reading you might change the title of \
    \ this page to 'Welcome to project-name' and add some personalized instructions for your team."

----------------------------------------------------------------------------------------------------
-- Accumulated blocks
----------------------------------------------------------------------------------------------------

getStartedBlock :: [Block]
getStartedBlock =
    [ getStarted
    , startedThumbnail
    , welcome
    , lineBreak
    , everythingIs
    , lineBreak
    , clickOn
    , lineBreak
    , pressTab
    , lineBreak
    , highlightText
    , addLinks
    , lineBreak
    ]

getsInterestingBlock :: [Block]
getsInterestingBlock =
    [ hereIs
    , lineBreak
    , clickNewLink
    , lineBreak
    , clickRelated
    , pagesThat
    , lineBreak
    , seeImages
    , lineBreak
    , ourGoalIs
    , lineBreak
    , lineBreak
    ]

onceStartedBlock :: [Block]
onceStartedBlock =
    [ whatCan
    , useScrapbox
    , lineBreak
    , forExample
    , letsSay
    , lineBreak
    , youCanImmediately
    , lineBreak
    , onceYoucan
    , lineBreak
    , whatIdeas
    , lineBreak
    , lineBreak
    , onceYouGot
    , seeAList
    , includesMore
    , lineBreak
    , lineBreak
    , seeSome
    , includesSaas
    , lineBreak
    , howTos
    , forDetails
    , lineBreak
    , lineBreak
    , weWouldLove
    , pleaseLet
    , lineBreak
    , thankYouFor
    , thankYouThumbnail
    , lineBreak
    , lineBreak
    , noteWhen
    ]

-- | 'Markdown' of get started section
getStartedMd :: Markdown
getStartedMd = markdown getStartedBlock

-- | 'Markdown' of gets interesting section
getsInterestingMd :: Markdown
getsInterestingMd = markdown getsInterestingBlock

-- | 'Markdown' of once started section
onceStartedMd :: Markdown
onceStartedMd = markdown onceStartedBlock

-- | Example of how https://scrapbox.io/scrapbox-parser/Get_started should be parsed
getStartedMarkdown :: Markdown
getStartedMarkdown = markdown $ getStartedBlock <> getsInterestingBlock <> onceStartedBlock

-- "Get started",
-- "[https://gyazo.com/5f93e65a3b979ae5333aca4f32600611]",
-- "Welcome to your new Scrapbox project!",
-- "",
-- "[** 📝Everything is editable]",
-- "",
-- "\tClick on any line and start typing to edit. ",
-- "",
-- "\t\tPress tab at the beginning of a line to indent and add a bullet point.",
-- "",
-- " Highlight text to make it a [new link], [* bold], [- and] [/ more].",
-- "\t\tAdd links while typing with a `#` before or brackets around `[`words you want to link `]` ",
-- "",


-- "[** 🎯 Here is where it gets interesting ]",
-- "",
-- "\tClick a [new link] to create a new page with that title and open it.",
-- "",
-- "\tClick related thumbnails in the footer of any page to explore ideas you have linked.",
-- " \tPages that are directly linked or two steps away from the current page will be displayed.",
-- "",
-- "\tSee images, videos, and external links added inside `[` brackets`]` on the page",
-- "",
-- "> Our goal is to help you build a map of your ideas that gains clarity and context with every scrap you add. ",
-- "",
-- "",
-- "[* What can you put in a Scrapbox project?]",
-- "\tUse Scrapbox to outline ideas, discuss `code blocks`, give feedback, and brainstorm. ",
-- "",
-- "[* For example]",
-- "\tLets say you are working on developing a new website. You might want to discuss ideas with your team before and while you execute the plan.  First create a page `Site plan` to start a conversation about the site requirements and link some useful resources. On that page you might add a link for a new page called `Social media buttons`.",
-- "",
-- "\tYou can immediately click on that link to `Social media buttons` and start editing.  There you may add links to `Twitter`, `Facebook`, etc.  Next you can click on `Twitter` and you'll see a related link that will take you back to `Site Plan`. ",
-- "",
-- "Once you can easily and directly type your ideas while also building context ideas become more clear the more you use it. No more folders full of dead text means no more teams isolated from their own ideas.",
-- "",
-- ">  [/ What ideas in your head could your team benefit from you putting down right now? Go create your first three or so pages and add a few links. From 3 to 3,000 pages your ideas will only grow in context.]",
-- "",
-- "",
-- "[** 📌 Once you've got the basics, here are ways to dig deeper and get the most out of your new project ]",
-- " See a list of all the [https://scrapbox.io/help/Things%20you%20can%20do Things you can do] ",
-- " \tIncludes more syntax, inviting team members, and creating profiles",
-- "",
-- "\tSee some [https://scrapbox.io/help/examples Example projects] ",
-- " \tIncludes a SaaS startup, design agency, and more",
-- "",
-- "\tSee [https://scrapbox.io/help/ How-tos and support] ",
-- " \tFor detailed instructions and answers to FAQs",
-- "",
-- "",
-- "[* We would love to hear any questions or feedback you may have]",
-- "Please let us know if you have any suggestions, questions, or points of friction.You can contact us directly by email: contact@scrapbox.io, [twitter https://twitter.com/scrapboxapp], and [https://facebook.com/scrapboxapp facebook]",
-- "",
-- "[/ Thank you for using Scrapbox!]",
-- "[https://gyazo.com/5aeffb3e8a6561ae78430664d8257f58]",
-- "",
-- "",
-- ">Note: When you're done reading you might change the title of this page to 'Welcome to project-name' and add some personalized instructions for your team.",
