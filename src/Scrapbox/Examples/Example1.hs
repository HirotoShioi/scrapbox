{-| Example of how our defined AST can be used to represent the scrapbox page

Example page: https://scrapbox.io/scrapbox-parser/Get_started
-}

{-# LANGUAGE OverloadedStrings #-}

module Scrapbox.Examples.Example1
  ( getStartedScrapbox
  , getsInterestingSB
  , onceStartedSB
  , getStartedSB
  ) where

import           RIO                   hiding (link, span)

import           Scrapbox.Constructors (blockQuote, bold, bulletPoint,
                                        codeNotation, heading, italic,
                                        lineBreak, link, p, scrapbox, span,
                                        strikeThrough, text, thumbnail)
import           Scrapbox.Types        (Block, Scrapbox)

--------------------------------------------------------------------------------
-- SECTION: Get started
--------------------------------------------------------------------------------

-- "Get started",
getStarted :: Block
getStarted = p [span [] [text "Get started"]]

-- "[https://gyazo.com/5f93e65a3b979ae5333aca4f32600611]",
startedThumbnail :: Block
startedThumbnail = thumbnail "https://gyazo.com/5f93e65a3b979ae5333aca4f32600611"

-- "Welcome to your new Scrapbox project!",
welcome :: Block
welcome = p [span [] [text "Welcome to your new Scrapbox project!"]]

-- "[** 📝Everything is editable]"
everythingIs :: Block
everythingIs = heading 2 [text "📝 Everything is editable"]

-- "\tClick on any line and start typing to edit. "
clickOn :: Block
clickOn = bulletPoint 1
    [ p
        [span [] [text "Click on any line and start typing to edit. "]]
    ]

-- "\t\tPress tab at the beginning of a line to indent and add a bullet point."
pressTab :: Block
pressTab = bulletPoint 2
    [ p
        [span [] [text "Press tab at the beginning of a line to indent and add a bullet point."]]
    ]

-- " Highlight text to make it a [new link], [* bold], [- and] [/ more]."
highlightText :: Block
highlightText = bulletPoint 1
    [ p
        [ span []
            [ text " Highlight text to make it a "
            , link Nothing "New link"
            , text ", "
            ]
        , span [bold] [text "bold"]
        , span [] [text ", "]
        , span [strikeThrough] [text "and"]
        , span [] [text " "]
        , span [italic] [text "more"]
        , span [] [text "."]
        ]
    ]

-- "\t\tAdd links while typing with a `#` before or brackets around `[`words you want to link `]` "
addLinks :: Block
addLinks = bulletPoint 2
    [ p
        [ span [] [ text "Add links while typing with a "]
        , codeNotation "#"
        , span [] [text " before or brackets around "]
        , codeNotation "["
        , span [] [text "words you want to link "]
        , codeNotation "]"
        ]
    ]

----------------------------------------------------------------------------------------------------
-- SECTION: Here is where it gets interesting
----------------------------------------------------------------------------------------------------

-- "[** 🎯 Here is where it gets interesting ]",
hereIs :: Block
hereIs = heading 2 [text "🎯 Here is where it gets interesting "]

-- "\tClick a [new link] to create a new page with that title and open it.",
clickNewLink :: Block
clickNewLink =
    bulletPoint 1
        [ p
            [ span []
                [ text "Click a "
                , link Nothing "New Link"
                , text " to create a new page with that title and open it."
                ]
            ]
        ]

-- Click related thumbnails in the footer of any page to explore ideas you have linked.
clickRelated :: Block
clickRelated = p [span [] [text "Click related thumbnails in the footer of any page to explore ideas \
    \you have linked." ]]

-- " \tPages that are directly linked or two steps away from the current page will be displayed.",
pagesThat :: Block
pagesThat = bulletPoint 2
    [ p
        [ span []
            [text "Pages that are directly linked or two steps away from the current page \
            \will be displayed."
            ]
        ]
    ]

-- "\tSee images, videos, and external links added inside `[` brackets`]` on the page",
seeImages :: Block
seeImages = bulletPoint 1
    [ p
        [ span [] [ text "See images, videos, and external links added inside "]
        , codeNotation "["
        , span [] [text " brackets"]
        , codeNotation  "]"
        , span [] [text " on the page"]
        ]
    ]

-- "> Our goal is to help you build a map of your ideas that gains clarity and context with every scrap you add. ",
ourGoalIs :: Block
ourGoalIs = blockQuote [span [] [ourGoalItext]]
  where
    ourGoalItext = text " Our goal is to help you build a map of your ideas that gains\
    \ clarity and context with every scrap you add. "

----------------------------------------------------------------------------------------------------
-- SECTION: What can you put in a Scrapbox project?
----------------------------------------------------------------------------------------------------

-- "[* What can you put in a Scrapbox project?]",
whatCan :: Block
whatCan = heading 1 [text "What can you put in a Scrapbox project?"]

-- "\tUse Scrapbox to outline ideas, discuss `code blocks`, give feedback, and brainstorm. ",
useScrapbox :: Block
useScrapbox = bulletPoint 1
    [ p
        [ span [] [text "Use Scrapbox to outline ideas, discuss "]
        , codeNotation "code blocks"
        , span [] [text ", give feedback, and brainstorm. "]
        ]
    ]

-- "[* For example]",
forExample :: Block
forExample = bulletPoint 1 [p [span [] [text "For example"]]]

-- "\tLets say you are working on developing a new website. You might want to discuss ideas with
-- your team before and while you execute the plan.  First create a page `Site plan` to start a
-- conversation about the site requirements and link some useful resources. On that page you might
-- add a link for a new page called `Social media buttons`.",
letsSay :: Block
letsSay = bulletPoint 1 [ p [letsSayText, sitePlan, toStart, socialMedia, period]]
  where
    letsSayText = span [] [text "Lets say you are working on developing a new website. \
    \You might want to discuss ideas with your team before and while you execute the plan.  First create a page "]
    sitePlan    = codeNotation "Site plan"
    toStart     = span [] [text " to start a conversation about the site requirements and link \
    \some useful resources. On that page you might add a link for a new page called "]
    socialMedia = codeNotation "Social media buttons"
    period      = span [] [text "."]

-- "\tYou can immediately click on that link to `Social media buttons` and start editing.
-- There you may add links to `Twitter`, `Facebook`, etc.  Next you can click on `Twitter` and you'll
-- see a related link that will take you back to `Site Plan`. ",
youCanImmediately :: Block
youCanImmediately = bulletPoint 1
    [ p
        [ youcan
        , socialMedia
        , andStart
        , twitter
        , column
        , faceBook
        , nextYoucan
        , twitter
        , relatedLink
        , sitePlan
        , period
        ]
    ]
  where
    youcan      = span [] [text "You can immediately click on that link to `Social media buttons` and \
    \ start editing. There you may add links to "]
    socialMedia = codeNotation "Social media buttons"
    twitter     = codeNotation "Twitter"
    faceBook    = codeNotation "Facebook"
    andStart    = span [] [text " and start editing.  There you may add links to "]
    column      = span [] [text ", "]
    nextYoucan  = span [] [text ", etc.  Next you can click on "]
    relatedLink = span [] [text " and you'll see a related link that will take you back to "]
    sitePlan    = codeNotation "Site Plan"
    period      = span [] [text ". "]

-- Once you can easily and directly type your ideas while also building context ideas become more
-- clear the more you use it. No more folders full of dead text means no more teams isolated from their own ideas.
onceYoucan :: Block
onceYoucan = p [span [] [text "Once you can easily and directly type your ideas while also building \
\ context ideas become more clear the more you use it. No more folders full of dead text means no \
\more teams isolated from their own ideas."]]

-- >  [/ What ideas in your head could your team benefit from you putting down right now? Go create
--  your first three or so pages and add a few links. From 3 to 3,000 pages your ideas will only grow in context.]
whatIdeas :: Block
whatIdeas = blockQuote [span [italic] [text "What ideas in your head could your team \
    \ benefit from you putting down right now? Go create your first three or so pages and add a few \
    \links. From 3 to 3,000 pages your ideas will only grow in context."]]

-- "[** 📌 Once you've got the basics, here are ways to dig deeper and get the most out of your new project ]",
onceYouGot :: Block
onceYouGot = heading 2 [text "📌 Once you've got the basics, here are ways to dig deeper and \
  \get the most out of your new project "]


-- " See a list of all the [https://scrapbox.io/help/Things%20you%20can%20do Things you can do] ",
seeAList :: Block
seeAList = p
    [ span []
        [ text " See a list of all the "
        , link (Just "Things you can do") "https://scrapbox.io/help/Things%20you%20can%20do"
        , text " "
        ]
    ]

-- " \tIncludes more syntax, inviting team members, and creating profiles",
includesMore :: Block
includesMore = bulletPoint 1
    [ p
        [ span []
            [ text "Includes more syntax, inviting team members, and creating profiles"]
        ]
    ]

-- "\tSee some [https://scrapbox.io/help/examples Example projects] ",
seeSome :: Block
seeSome = bulletPoint 1
    [ p
        [ span []
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
        [ span []
            [text "Includes a SaaS startup, design agency, and more"]
        ]
    ]


-- "\tSee [https://scrapbox.io/help/ How-tos and support] ",
howTos :: Block
howTos = bulletPoint 1
    [ p
        [span []
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
        [span [] [text "For detailed instructions and answers to FAQs"]]
    ]

-- "[* We would love to hear any questions or feedback you may have]",
weWouldLove :: Block
weWouldLove = heading 1 [text "We would love to hear any questions or feedback you may have"]

-- "Please let us know if you have any suggestions, questions, or points of friction.You can contact us directly by email: contact@scrapbox.io, [twitter https://twitter.com/scrapboxapp], and [https://facebook.com/scrapboxapp facebook]",
pleaseLet :: Block
pleaseLet = p
    [span []
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
thankYouFor = p [span [italic] [text "Thank you for using Scrapbox!"]]

-- "[https://gyazo.com/5aeffb3e8a6561ae78430664d8257f58]",
thankYouThumbnail :: Block
thankYouThumbnail = thumbnail "https://gyazo.com/5aeffb3e8a6561ae78430664d8257f58"

-- ">Note: When you're done reading you might change the title of this page to 'Welcome to
-- project-name' and add some personalized instructions for your team.",
noteWhen :: Block
noteWhen = blockQuote [span [] [noteWhenText]]
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

-- | 'Scrapbox' of get started section
getStartedSB :: Scrapbox
getStartedSB = scrapbox getStartedBlock

-- | 'Scrapbox' of gets interesting section
getsInterestingSB :: Scrapbox
getsInterestingSB = scrapbox getsInterestingBlock

-- | 'Scrapbox' of once started section
onceStartedSB :: Scrapbox
onceStartedSB = scrapbox onceStartedBlock

-- | Example of how https://scrapbox.io/scrapbox-parser/Get_started should be parsed
getStartedScrapbox :: Scrapbox
getStartedScrapbox = scrapbox $ getStartedBlock <> getsInterestingBlock <> onceStartedBlock

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
